#!/usr/bin/env php
<?php

declare(strict_types=1);

use PhpParser\Error;
use PhpParser\Node;
use PhpParser\NodeTraverser;
use PhpParser\NodeVisitor\NameResolver;
use PhpParser\ParserFactory;
use PhpParser\PhpVersion;

// [versionsync: PHP_VERSION=8.4.19]
const TARGET_PHP_VERSION = '8.4.19';
const SCHEMA_VERSION = 16;

final class LoweringError extends RuntimeException
{
}

final class Lowerer
{
    /** @var list<int> */
    private array $lineStarts;

    public function __construct(
        private readonly string $filename,
        private readonly string $source,
    ) {
        $this->lineStarts = [0];
        $length = strlen($source);
        for ($offset = 0; $offset < $length; ++$offset) {
            if ($source[$offset] === "\n") {
                $this->lineStarts[] = $offset + 1;
            }
        }
    }

    /** @param list<Node\Stmt> $statements */
    public function lowerProgram(array $statements): array
    {
        $statements = $this->flattenProgramStatements($statements);
        $functions = [];
        $functionNames = [];
        $classes = [];
        $classNames = [];
        $body = [];
        foreach ($statements as $statement) {
            if ($statement instanceof Node\Stmt\Function_) {
                $function = $this->lowerFunction($statement);
                $canonicalName = strtolower($function['name']);
                if (array_key_exists($canonicalName, $functionNames)) {
                    return $this->unsupported(
                        $statement,
                        'duplicate function declaration',
                    );
                }
                $functionNames[$canonicalName] = true;
                $functions[] = $function;
            } elseif (
                $statement instanceof Node\Stmt\Class_
                || $statement instanceof Node\Stmt\Interface_
                || $statement instanceof Node\Stmt\Trait_
            ) {
                $class = $this->lowerClassLike($statement);
                $canonicalName = strtolower($class['name']);
                if (array_key_exists($canonicalName, $classNames)) {
                    return $this->unsupported(
                        $statement,
                        'duplicate class declaration',
                    );
                }
                $classNames[$canonicalName] = true;
                $classes[] = $class;
            } elseif (
                $statement instanceof Node\Stmt\Expression
                && $statement->expr instanceof Node\Expr\Include_
            ) {
                $body[] = $this->lowerInclude($statement->expr, $statement);
            } else {
                $body[] = $this->lowerStatement($statement, false, 0);
            }
        }

        return [
            'schema_version' => SCHEMA_VERSION,
            'target_php_version' => TARGET_PHP_VERSION,
            'source_file' => $this->filename,
            'source_files' => [$this->filename],
            'functions' => $functions,
            'classes' => $classes,
            'statements' => $body,
        ];
    }

    /**
     * @param list<Node\Stmt> $statements
     * @return list<Node\Stmt>
     */
    private function flattenProgramStatements(array $statements): array
    {
        $flattened = [];
        foreach ($statements as $statement) {
            if ($statement instanceof Node\Stmt\Namespace_) {
                array_push(
                    $flattened,
                    ...$this->flattenProgramStatements($statement->stmts),
                );
            } elseif (
                $statement instanceof Node\Stmt\Use_
                || $statement instanceof Node\Stmt\GroupUse
            ) {
                continue;
            } elseif ($statement instanceof Node\Stmt\Declare_) {
                if (
                    $statement->stmts !== null
                    || count($statement->declares) !== 1
                    || strtolower($statement->declares[0]->key->toString()) !== 'strict_types'
                    || !($statement->declares[0]->value instanceof Node\Scalar\Int_)
                    || $statement->declares[0]->value->value !== 1
                ) {
                    return $this->unsupported($statement, 'declare statement');
                }
            } else {
                $flattened[] = $statement;
            }
        }
        return $flattened;
    }

    private function lowerInclude(
        Node\Expr\Include_ $include,
        Node\Stmt\Expression $statement,
    ): array {
        $type = match ($include->type) {
            Node\Expr\Include_::TYPE_INCLUDE => 'include',
            Node\Expr\Include_::TYPE_INCLUDE_ONCE => 'include_once',
            Node\Expr\Include_::TYPE_REQUIRE => 'require',
            Node\Expr\Include_::TYPE_REQUIRE_ONCE => 'require_once',
        };
        $path = $this->includePath($include->expr);
        if (
            !str_starts_with($path, DIRECTORY_SEPARATOR)
            && !file_exists($path)
        ) {
            $path = dirname($this->filename) . DIRECTORY_SEPARATOR . $path;
        }
        return [
            'kind' => '__include',
            'type' => $type,
            'path' => $path,
            'location' => $this->location($statement),
        ];
    }

    private function includePath(Node\Expr $expression): string
    {
        if ($expression instanceof Node\Scalar\String_) {
            return $expression->value;
        }
        if ($expression instanceof Node\Scalar\MagicConst\Dir) {
            return dirname($this->filename);
        }
        if ($expression instanceof Node\Scalar\MagicConst\File) {
            return $this->filename;
        }
        if ($expression instanceof Node\Expr\BinaryOp\Concat) {
            return $this->includePath($expression->left)
                . $this->includePath($expression->right);
        }
        return $this->unsupported($expression, 'dynamic include path');
    }

    private function lowerClassLike(
        Node\Stmt\Class_|Node\Stmt\Interface_|Node\Stmt\Trait_ $class,
    ): array
    {
        if ($class->name === null || $class->attrGroups !== []) {
            return $this->unsupported($class, 'class declaration');
        }
        if ($class instanceof Node\Stmt\Class_ && $class->flags !== 0) {
            return $this->unsupported($class, 'class declaration');
        }

        $name = $class->namespacedName instanceof Node\Name
            ? $class->namespacedName->toString()
            : $class->name->toString();
        $kind = match (true) {
            $class instanceof Node\Stmt\Class_ => 'class',
            $class instanceof Node\Stmt\Interface_ => 'interface',
            $class instanceof Node\Stmt\Trait_ => 'trait',
        };
        $parent = $class instanceof Node\Stmt\Class_
            && $class->extends instanceof Node\Name
            ? $this->resolvedName($class->extends)
            : null;
        $interfaces = match (true) {
            $class instanceof Node\Stmt\Class_ => $class->implements,
            $class instanceof Node\Stmt\Interface_ => $class->extends,
            $class instanceof Node\Stmt\Trait_ => [],
        };
        $properties = [];
        $propertyNames = [];
        $methods = [];
        $methodNames = [];
        $traits = [];
        foreach ($class->stmts as $statement) {
            if ($statement instanceof Node\Stmt\Property) {
                if ($kind === 'interface') {
                    return $this->unsupported($statement, 'interface member');
                }
                foreach ($this->lowerProperties($statement) as $property) {
                    if (array_key_exists($property['name'], $propertyNames)) {
                        return $this->unsupported(
                            $statement,
                            'duplicate property declaration',
                        );
                    }
                    $propertyNames[$property['name']] = true;
                    $properties[] = $property;
                }
            } elseif ($statement instanceof Node\Stmt\ClassMethod) {
                $method = $this->lowerMethod($statement, $kind === 'interface');
                if (
                    $method['attributes'] !== []
                    && (
                        $kind !== 'class'
                        || $method['parameters'] !== []
                        || $method['modifiers'][0] !== 'public'
                    )
                ) {
                    return $this->unsupported(
                        $statement,
                        'Soteria\\Test method entry point',
                    );
                }
                $canonicalName = strtolower($method['name']);
                if (array_key_exists($canonicalName, $methodNames)) {
                    return $this->unsupported(
                        $statement,
                        'duplicate method declaration',
                    );
                }
                $methodNames[$canonicalName] = true;
                $methods[] = $method;
            } elseif ($statement instanceof Node\Stmt\TraitUse) {
                if ($kind === 'interface') {
                    return $this->unsupported($statement, 'interface member');
                }
                $traits[] = $this->lowerTraitUse($statement);
            } else {
                return $this->unsupported($statement, 'class member');
            }
        }

        return [
            'kind' => $kind,
            'name' => $name,
            'parent' => $parent,
            'interfaces' => array_map(
                fn (Node\Name $interface): string => $this->resolvedName($interface),
                $interfaces,
            ),
            'traits' => $traits,
            'properties' => $properties,
            'methods' => $methods,
            'location' => $this->location($class),
        ];
    }

    private function lowerTraitUse(Node\Stmt\TraitUse $use): array
    {
        $adaptations = [];
        foreach ($use->adaptations as $adaptation) {
            if ($adaptation instanceof Node\Stmt\TraitUseAdaptation\Precedence) {
                $adaptations[] = [
                    'kind' => 'precedence',
                    'trait' => $this->resolvedName($adaptation->trait),
                    'method' => $adaptation->method->toString(),
                    'instead_of' => array_map(
                        fn (Node\Name $trait): string => $this->resolvedName($trait),
                        $adaptation->insteadof,
                    ),
                    'location' => $this->location($adaptation),
                ];
            } elseif ($adaptation instanceof Node\Stmt\TraitUseAdaptation\Alias) {
                $visibility = $adaptation->newModifier === null
                    ? null
                    : $this->lowerVisibility($adaptation->newModifier);
                if ($adaptation->newModifier !== null && $visibility === null) {
                    return $this->unsupported($adaptation, 'trait alias modifier');
                }
                $adaptations[] = [
                    'kind' => 'alias',
                    'trait' => $adaptation->trait === null
                        ? null
                        : $this->resolvedName($adaptation->trait),
                    'method' => $adaptation->method->toString(),
                    'alias' => $adaptation->newName?->toString(),
                    'visibility' => $visibility,
                    'location' => $this->location($adaptation),
                ];
            } else {
                return $this->unsupported($adaptation, 'trait adaptation');
            }
        }

        return [
            'traits' => array_map(
                fn (Node\Name $trait): string => $this->resolvedName($trait),
                $use->traits,
            ),
            'adaptations' => $adaptations,
            'location' => $this->location($use),
        ];
    }

    private function lowerMethod(
        Node\Stmt\ClassMethod $method,
        bool $interfaceMethod,
    ): array
    {
        $visibilityMask = Node\Stmt\Class_::MODIFIER_PUBLIC
            | Node\Stmt\Class_::MODIFIER_PROTECTED
            | Node\Stmt\Class_::MODIFIER_PRIVATE;
        $allowedFlags = $visibilityMask
            | Node\Stmt\Class_::MODIFIER_STATIC
            | ($interfaceMethod ? Node\Stmt\Class_::MODIFIER_ABSTRACT : 0);
        $visibility = $this->lowerVisibility($method->flags & $visibilityMask);
        if (
            $method->byRef
            || $method->returnType !== null
            || ($interfaceMethod !== ($method->stmts === null))
            || ($method->flags & ~$allowedFlags) !== 0
            || $visibility === null
        ) {
            return $this->unsupported($method, 'method declaration');
        }
        $attributes = $this->lowerAttributes($method->attrGroups);

        $parameters = [];
        $parameterNames = [];
        foreach ($method->params as $parameter) {
            $lowered = $this->lowerParameter($parameter);
            if (array_key_exists($lowered['name'], $parameterNames)) {
                return $this->unsupported(
                    $parameter,
                    'duplicate method parameter',
                );
            }
            if ($lowered['name'] === 'this') {
                return $this->unsupported($parameter, 'method parameter named this');
            }
            $parameterNames[$lowered['name']] = true;
            $parameters[] = $lowered;
        }

        return [
            'name' => $method->name->toString(),
            'parameters' => $parameters,
            'body' => $method->stmts === null
                ? null
                : array_map(
                    fn (Node\Stmt $statement): array => $this->lowerStatement(
                        $statement,
                        true,
                        0,
                    ),
                    $method->stmts,
                ),
            'modifiers' => array_values(array_filter([
                $visibility,
                ($method->flags & Node\Stmt\Class_::MODIFIER_STATIC) !== 0
                    ? 'static'
                    : null,
            ])),
            'attributes' => $attributes,
            'location' => $this->location($method),
        ];
    }

    private function lowerProperties(Node\Stmt\Property $property): array
    {
        $visibilityMask = Node\Stmt\Class_::MODIFIER_PUBLIC
            | Node\Stmt\Class_::MODIFIER_PROTECTED
            | Node\Stmt\Class_::MODIFIER_PRIVATE;
        $allowedFlags = $visibilityMask | Node\Stmt\Class_::MODIFIER_STATIC;
        $visibility = $this->lowerVisibility($property->flags & $visibilityMask);
        if (
            $visibility === null
            || ($property->flags & ~$allowedFlags) !== 0
            || $property->type !== null
            || $property->attrGroups !== []
            || $property->hooks !== []
        ) {
            return $this->unsupported($property, 'property declaration');
        }

        return array_map(
            function (Node\PropertyItem $item) use ($visibility, $property): array {
                if (
                    $item->default !== null
                    && !$this->isSupportedPropertyDefault($item->default)
                ) {
                    return $this->unsupported($item->default, 'property default');
                }
                return [
                    'name' => $item->name->toString(),
                    'default' => $item->default === null
                        ? null
                        : $this->lowerExpression($item->default),
                    'modifiers' => array_values(array_filter([
                        $visibility,
                        ($property->flags & Node\Stmt\Class_::MODIFIER_STATIC) !== 0
                            ? 'static'
                            : null,
                    ])),
                    'location' => $this->location($item),
                ];
            },
            $property->props,
        );
    }

    private function lowerVisibility(int $flags): ?string
    {
        return match ($flags) {
            0, Node\Stmt\Class_::MODIFIER_PUBLIC => 'public',
            Node\Stmt\Class_::MODIFIER_PROTECTED => 'protected',
            Node\Stmt\Class_::MODIFIER_PRIVATE => 'private',
            default => null,
        };
    }

    private function isSupportedPropertyDefault(Node\Expr $expression): bool
    {
        if (
            $expression instanceof Node\Scalar\Int_
            || $expression instanceof Node\Scalar\Float_
            || $expression instanceof Node\Scalar\String_
        ) {
            return true;
        }
        if ($expression instanceof Node\Expr\ConstFetch) {
            return in_array(
                strtolower($expression->name->toString()),
                ['null', 'true', 'false'],
                true,
            );
        }
        if (
            $expression instanceof Node\Expr\UnaryPlus
            || $expression instanceof Node\Expr\UnaryMinus
        ) {
            return $this->isSupportedNumericPropertyDefault($expression->expr);
        }
        if ($expression instanceof Node\Expr\Array_) {
            foreach ($expression->items as $item) {
                if (
                    $item === null
                    || $item->unpack
                    || $item->byRef
                    || ($item->key !== null
                        && !$this->isSupportedPropertyDefault($item->key))
                    || !$this->isSupportedPropertyDefault($item->value)
                ) {
                    return false;
                }
            }
            return true;
        }

        return false;
    }

    private function isSupportedNumericPropertyDefault(
        Node\Expr $expression,
    ): bool
    {
        if (
            $expression instanceof Node\Scalar\Int_
            || $expression instanceof Node\Scalar\Float_
        ) {
            return true;
        }
        if (
            $expression instanceof Node\Expr\UnaryPlus
            || $expression instanceof Node\Expr\UnaryMinus
        ) {
            return $this->isSupportedNumericPropertyDefault($expression->expr);
        }

        return false;
    }

    private function lowerFunction(Node\Stmt\Function_ $function): array
    {
        if ($function->byRef) {
            return $this->unsupported($function, 'by-reference function return');
        }
        if ($function->returnType !== null) {
            return $this->unsupported($function->returnType, 'function return type');
        }
        $attributes = $this->lowerAttributes($function->attrGroups);

        $name = $function->namespacedName instanceof Node\Name
            ? $function->namespacedName->toString()
            : $function->name->toString();

        $parameters = [];
        $parameterNames = [];
        foreach ($function->params as $parameter) {
            $lowered = $this->lowerParameter($parameter);
            if (array_key_exists($lowered['name'], $parameterNames)) {
                return $this->unsupported(
                    $parameter,
                    'duplicate function parameter',
                );
            }
            $parameterNames[$lowered['name']] = true;
            $parameters[] = $lowered;
        }
        if ($attributes !== [] && $parameters !== []) {
            return $this->unsupported(
                $function,
                'Soteria\\Test function entry point',
            );
        }

        return [
            'name' => $name,
            'parameters' => $parameters,
            'body' => array_map(
                fn (Node\Stmt $statement): array => $this->lowerStatement(
                    $statement,
                    true,
                    0,
                ),
                $function->stmts,
            ),
            'attributes' => $attributes,
            'location' => $this->location($function),
        ];
    }

    /**
     * @param list<Node\AttributeGroup> $groups
     * @return list<array{name: string, location: array}>
     */
    private function lowerAttributes(array $groups): array
    {
        $attributes = [];
        foreach ($groups as $group) {
            foreach ($group->attrs as $attribute) {
                $name = $this->resolvedName($attribute->name);
                if (
                    strtolower($name) !== 'soteria\\test'
                    || $attribute->args !== []
                    || $attributes !== []
                ) {
                    return $this->unsupported($attribute, 'attribute');
                }
                $attributes[] = [
                    'name' => $name,
                    'location' => $this->location($attribute),
                ];
            }
        }
        return $attributes;
    }

    private function lowerParameter(Node\Param $parameter): array
    {
        if (
            $parameter->default !== null
            || $parameter->type !== null
            || $parameter->byRef
            || $parameter->variadic
            || $parameter->flags !== 0
            || $parameter->attrGroups !== []
            || $parameter->hooks !== []
            || !($parameter->var instanceof Node\Expr\Variable)
            || !is_string($parameter->var->name)
            || $parameter->var->name === 'this'
        ) {
            return $this->unsupported($parameter, 'function parameter');
        }

        return [
            'name' => $parameter->var->name,
            'location' => $this->location($parameter),
        ];
    }

    private function lowerStatement(
        Node\Stmt $statement,
        bool $inFunction,
        int $loopDepth,
    ): array
    {
        $location = $this->location($statement);

        return match (true) {
            $statement instanceof Node\Stmt\Expression => [
                'kind' => 'expression',
                'expression' => $this->lowerExpression($statement->expr),
                'location' => $location,
            ],
            $statement instanceof Node\Stmt\Echo_ => [
                'kind' => 'echo',
                'expressions' => array_map(
                    $this->lowerExpression(...),
                    $statement->exprs,
                ),
                'location' => $location,
            ],
            $statement instanceof Node\Stmt\If_ => $this->lowerIf(
                $statement,
                $location,
                $inFunction,
                $loopDepth,
            ),
            $statement instanceof Node\Stmt\While_ => [
                'kind' => 'while',
                'condition' => $this->lowerExpression($statement->cond),
                'body' => array_map(
                    fn (Node\Stmt $bodyStatement): array => $this->lowerStatement(
                        $bodyStatement,
                        $inFunction,
                        $loopDepth + 1,
                    ),
                    $statement->stmts,
                ),
                'location' => $location,
            ],
            $statement instanceof Node\Stmt\Foreach_ => $this->lowerForeach(
                $statement,
                $location,
                $inFunction,
                $loopDepth,
            ),
            $statement instanceof Node\Stmt\Break_ => [
                'kind' => 'break',
                'depth' => $this->lowerLoopControlDepth(
                    $statement,
                    $statement->num,
                    $loopDepth,
                ),
                'location' => $location,
            ],
            $statement instanceof Node\Stmt\Continue_ => [
                'kind' => 'continue',
                'depth' => $this->lowerLoopControlDepth(
                    $statement,
                    $statement->num,
                    $loopDepth,
                ),
                'location' => $location,
            ],
            $inFunction && $statement instanceof Node\Stmt\Return_ => [
                'kind' => 'return',
                'expression' => $statement->expr === null
                    ? null
                    : $this->lowerExpression($statement->expr),
                'location' => $location,
            ],
            $statement instanceof Node\Stmt\TryCatch => $this->lowerTry(
                $statement,
                $location,
                $inFunction,
                $loopDepth,
            ),
            $statement instanceof Node\Stmt\Unset_ => [
                'kind' => 'unset',
                'targets' => array_map(
                    fn (Node\Expr $target): array => $this->lowerLvalue(
                        $target,
                        false,
                    ),
                    $statement->vars,
                ),
                'location' => $location,
            ],
            $statement instanceof Node\Stmt\Nop => [
                'kind' => 'nop',
                'location' => $location,
            ],
            default => $this->unsupported($statement, 'statement'),
        };
    }

    private function lowerForeach(
        Node\Stmt\Foreach_ $statement,
        array $location,
        bool $inFunction,
        int $loopDepth,
    ): array
    {
        return [
            'kind' => 'foreach',
            'iterable' => $this->lowerExpression($statement->expr),
            'key' => $statement->keyVar === null
                ? null
                : $this->lowerLvalue($statement->keyVar, true),
            'value' => $this->lowerLvalue($statement->valueVar, true),
            'by_reference' => $statement->byRef,
            'body' => array_map(
                fn (Node\Stmt $bodyStatement): array => $this->lowerStatement(
                    $bodyStatement,
                    $inFunction,
                    $loopDepth + 1,
                ),
                $statement->stmts,
            ),
            'location' => $location,
        ];
    }

    private function lowerIf(
        Node\Stmt\If_ $statement,
        array $location,
        bool $inFunction,
        int $loopDepth,
    ): array
    {
        if ($statement->elseifs !== []) {
            return $this->unsupported($statement->elseifs[0], 'elseif clause');
        }

        return [
            'kind' => 'if',
            'condition' => $this->lowerExpression($statement->cond),
            'then' => array_map(
                fn (Node\Stmt $thenStatement): array => $this->lowerStatement(
                    $thenStatement,
                    $inFunction,
                    $loopDepth,
                ),
                $statement->stmts,
            ),
            'else' => $statement->else === null
                ? []
                : array_map(
                    fn (Node\Stmt $elseStatement): array => $this->lowerStatement(
                        $elseStatement,
                        $inFunction,
                        $loopDepth,
                    ),
                    $statement->else->stmts,
                ),
            'location' => $location,
        ];
    }

    private function lowerLoopControlDepth(
        Node\Stmt $statement,
        ?Node\Expr $depth,
        int $loopDepth,
    ): int
    {
        $depth = $depth === null ? 1 : (
            $depth instanceof Node\Scalar\Int_ ? $depth->value : 0
        );
        if ($depth < 1 || $depth > $loopDepth) {
            return $this->unsupported($statement, 'loop-control depth');
        }

        return $depth;
    }

    private function lowerTry(
        Node\Stmt\TryCatch $statement,
        array $location,
        bool $inFunction,
        int $loopDepth,
    ): array
    {
        return [
            'kind' => 'try',
            'body' => array_map(
                fn (Node\Stmt $bodyStatement): array => $this->lowerStatement(
                    $bodyStatement,
                    $inFunction,
                    $loopDepth,
                ),
                $statement->stmts,
            ),
            'catches' => array_map(
                fn (Node\Stmt\Catch_ $catch): array => $this->lowerCatch(
                    $catch,
                    $inFunction,
                    $loopDepth,
                ),
                $statement->catches,
            ),
            'finally' => $statement->finally === null
                ? null
                : array_map(
                    fn (Node\Stmt $finallyStatement): array =>
                        $this->lowerStatement(
                            $finallyStatement,
                            $inFunction,
                            $loopDepth,
                        ),
                    $statement->finally->stmts,
                ),
            'location' => $location,
        ];
    }

    private function lowerCatch(
        Node\Stmt\Catch_ $catch,
        bool $inFunction,
        int $loopDepth,
    ): array
    {
        if ($catch->var !== null && !is_string($catch->var->name)) {
            return $this->unsupported($catch->var, 'catch variable');
        }

        return [
            'types' => array_map(
                fn (Node\Name $type): string => $this->resolvedName($type),
                $catch->types,
            ),
            'variable' => $catch->var?->name,
            'body' => array_map(
                fn (Node\Stmt $bodyStatement): array => $this->lowerStatement(
                    $bodyStatement,
                    $inFunction,
                    $loopDepth,
                ),
                $catch->stmts,
            ),
            'location' => $this->location($catch),
        ];
    }

    private function resolvedName(Node\Name $name): string
    {
        $resolvedName = $name->getAttribute('resolvedName');
        return $resolvedName instanceof Node\Name
            ? $resolvedName->toString()
            : $name->toString();
    }

    private function lowerExpression(Node\Expr $expression): array
    {
        $location = $this->location($expression);

        if ($expression instanceof Node\Scalar\Int_) {
            return [
                'kind' => 'int',
                'value' => (string) $expression->value,
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Scalar\Float_) {
            if (!is_finite($expression->value)) {
                return $this->unsupported($expression, 'non-finite float literal');
            }

            return [
                'kind' => 'float',
                'value' => sprintf('%.17g', $expression->value),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Scalar\String_) {
            if (preg_match('//u', $expression->value) !== 1) {
                return $this->unsupported($expression, 'non-UTF-8 string literal');
            }

            return [
                'kind' => 'string',
                'value' => $expression->value,
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\ConstFetch) {
            $name = strtolower($expression->name->toString());
            return match ($name) {
                'null' => ['kind' => 'null', 'location' => $location],
                'true' => [
                    'kind' => 'bool',
                    'value' => true,
                    'location' => $location,
                ],
                'false' => [
                    'kind' => 'bool',
                    'value' => false,
                    'location' => $location,
                ],
                default => $this->unsupported($expression, 'constant fetch'),
            };
        }

        if ($expression instanceof Node\Expr\Variable) {
            if (!is_string($expression->name)) {
                return $this->unsupported($expression, 'dynamic variable');
            }

            return [
                'kind' => 'variable',
                'name' => $expression->name,
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\Array_) {
            $items = [];
            foreach ($expression->items as $item) {
                if ($item === null || $item->unpack || $item->byRef) {
                    return $this->unsupported(
                        $item ?? $expression,
                        'array item',
                    );
                }
                $items[] = [
                    'key' => $item->key === null
                        ? null
                        : $this->lowerExpression($item->key),
                    'value' => $this->lowerExpression($item->value),
                    'location' => $this->location($item),
                ];
            }

            return [
                'kind' => 'array',
                'items' => $items,
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\ArrayDimFetch) {
            if ($expression->dim === null) {
                return $this->unsupported($expression, 'array append read');
            }

            return [
                'kind' => 'array_get',
                'target' => $this->lowerLvalue($expression, false),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\PropertyFetch) {
            if (!($expression->name instanceof Node\Identifier)) {
                return $this->unsupported($expression->name, 'dynamic property');
            }

            return [
                'kind' => 'property_get',
                'target' => $this->lowerLvalue($expression, false),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\Isset_) {
            return [
                'kind' => 'isset',
                'targets' => array_map(
                    fn (Node\Expr $target): array => $this->lowerLvalue(
                        $target,
                        false,
                    ),
                    $expression->vars,
                ),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\StaticPropertyFetch) {
            return [
                'kind' => 'property_get',
                'target' => $this->lowerLvalue($expression, false),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\AssignRef) {
            return [
                'kind' => 'assign_reference',
                'target' => $this->lowerLvalue($expression->var),
                'source' => $this->lowerLvalue($expression->expr),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\Assign) {
            return [
                'kind' => 'assign',
                'target' => $this->lowerLvalue($expression->var),
                'value' => $this->lowerExpression($expression->expr),
                'location' => $location,
            ];
        }

        $unaryOperator = match (true) {
            $expression instanceof Node\Expr\BooleanNot => 'boolean_not',
            $expression instanceof Node\Expr\UnaryPlus => 'numeric_identity',
            $expression instanceof Node\Expr\UnaryMinus => 'numeric_negation',
            default => null,
        };
        if ($unaryOperator !== null) {
            return [
                'kind' => 'unary',
                'operator' => $unaryOperator,
                'operand' => $this->lowerExpression($expression->expr),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\BinaryOp) {
            return [
                'kind' => 'binary',
                'operator' => $this->lowerBinaryOperator($expression),
                'left' => $this->lowerExpression($expression->left),
                'right' => $this->lowerExpression($expression->right),
                'location' => $location,
            ];
        }

        $cast = match (true) {
            $expression instanceof Node\Expr\Cast\Bool_ => 'bool',
            $expression instanceof Node\Expr\Cast\Int_ => 'int',
            $expression instanceof Node\Expr\Cast\Double => 'float',
            $expression instanceof Node\Expr\Cast\String_ => 'string',
            default => null,
        };
        if ($cast !== null) {
            return [
                'kind' => 'cast',
                'type' => $cast,
                'expression' => $this->lowerExpression($expression->expr),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\Closure) {
            if (
                $expression->static
                || $expression->byRef
                || $expression->returnType !== null
                || $expression->attrGroups !== []
            ) {
                return $this->unsupported($expression, 'closure declaration');
            }
            $parameters = [];
            $parameterNames = [];
            foreach ($expression->params as $parameter) {
                $lowered = $this->lowerParameter($parameter);
                if (array_key_exists($lowered['name'], $parameterNames)) {
                    return $this->unsupported($parameter, 'duplicate closure parameter');
                }
                $parameterNames[$lowered['name']] = true;
                $parameters[] = $lowered;
            }
            $captures = [];
            $captureNames = [];
            foreach ($expression->uses as $capture) {
                if (
                    !($capture->var instanceof Node\Expr\Variable)
                    || !is_string($capture->var->name)
                    || array_key_exists($capture->var->name, $captureNames)
                    || array_key_exists($capture->var->name, $parameterNames)
                    || $capture->var->name === 'this'
                ) {
                    return $this->unsupported($capture, 'closure capture');
                }
                $captureNames[$capture->var->name] = true;
                $captures[] = [
                    'name' => $capture->var->name,
                    'by_reference' => $capture->byRef,
                    'location' => $this->location($capture),
                ];
            }

            return [
                'kind' => 'closure',
                'parameters' => $parameters,
                'captures' => $captures,
                'body' => array_map(
                    fn (Node\Stmt $statement): array => $this->lowerStatement(
                        $statement,
                        true,
                        0,
                    ),
                    $expression->stmts,
                ),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\Throw_) {
            return [
                'kind' => 'throw',
                'expression' => $this->lowerExpression($expression->expr),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\New_) {
            if (!($expression->class instanceof Node\Name)) {
                return $this->unsupported(
                    $expression,
                    'dynamic class construction',
                );
            }
            foreach ($expression->args as $argument) {
                if (
                    !($argument instanceof Node\Arg)
                    || $argument->byRef
                    || $argument->unpack
                    || $argument->name !== null
                ) {
                    return $this->unsupported($argument, 'constructor argument');
                }
            }

            return [
                'kind' => 'new',
                'class' => $this->resolvedName($expression->class),
                'arguments' => array_map(
                    fn (Node\Arg $argument): array => $this->lowerExpression(
                        $argument->value,
                    ),
                    $expression->args,
                ),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\Clone_) {
            return [
                'kind' => 'clone',
                'expression' => $this->lowerExpression($expression->expr),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\MethodCall) {
            if (!($expression->name instanceof Node\Identifier)) {
                return $this->unsupported($expression->name, 'dynamic method call');
            }
            if ($this->isFirstClassCallable($expression->args)) {
                return [
                    'kind' => 'object_method_callable',
                    'object' => $this->lowerExpression($expression->var),
                    'method' => $expression->name->toString(),
                    'location' => $location,
                ];
            }
            foreach ($expression->args as $argument) {
                if (
                    !($argument instanceof Node\Arg)
                    || $argument->byRef
                    || $argument->unpack
                    || $argument->name !== null
                ) {
                    return $this->unsupported($argument, 'method argument');
                }
            }

            return [
                'kind' => 'method_call',
                'object' => $this->lowerExpression($expression->var),
                'method' => $expression->name->toString(),
                'arguments' => array_map(
                    fn (Node\Arg $argument): array => $this->lowerExpression(
                        $argument->value,
                    ),
                    $expression->args,
                ),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\StaticCall) {
            if (
                !($expression->class instanceof Node\Name)
                || !($expression->name instanceof Node\Identifier)
            ) {
                return $this->unsupported($expression, 'static method call');
            }
            $className = strtolower($expression->class->toString()) === 'parent'
                ? 'parent'
                : $this->resolvedName($expression->class);
            if ($this->isFirstClassCallable($expression->args)) {
                return [
                    'kind' => 'static_method_callable',
                    'class' => $className,
                    'method' => $expression->name->toString(),
                    'location' => $location,
                ];
            }
            foreach ($expression->args as $argument) {
                if (
                    !($argument instanceof Node\Arg)
                    || $argument->byRef
                    || $argument->unpack
                    || $argument->name !== null
                ) {
                    return $this->unsupported($argument, 'method argument');
                }
            }

            return [
                'kind' => $className === 'parent'
                    ? 'parent_method_call'
                    : 'static_method_call',
                ...($className === 'parent' ? [] : ['class' => $className]),
                'method' => $expression->name->toString(),
                'arguments' => array_map(
                    fn (Node\Arg $argument): array => $this->lowerExpression(
                        $argument->value,
                    ),
                    $expression->args,
                ),
                'location' => $location,
            ];
        }

        if ($expression instanceof Node\Expr\FuncCall) {
            if (
                $expression->name instanceof Node\Name
                && $this->isFirstClassCallable($expression->args)
            ) {
                return [
                    'kind' => 'function_callable',
                    'name' => $this->resolvedName($expression->name),
                    'location' => $location,
                ];
            }
            if ($this->isFirstClassCallable($expression->args)) {
                return $this->unsupported($expression, 'dynamic callable creation');
            }
            foreach ($expression->args as $argument) {
                if (
                    !($argument instanceof Node\Arg)
                    || $argument->byRef
                    || $argument->unpack
                    || $argument->name !== null
                ) {
                    return $this->unsupported($argument, 'function argument');
                }
            }

            return [
                'kind' => $expression->name instanceof Node\Name ? 'call' : 'invoke',
                ...($expression->name instanceof Node\Name
                    ? ['name' => $this->resolvedName($expression->name)]
                    : ['callee' => $this->lowerExpression($expression->name)]),
                'arguments' => array_map(
                    fn (Node\Arg $argument): array => $this->lowerExpression(
                        $argument->value,
                    ),
                    $expression->args,
                ),
                'location' => $location,
            ];
        }

        return $this->unsupported($expression, 'expression');
    }

    /** @param array<Node\Arg|Node\VariadicPlaceholder> $arguments */
    private function isFirstClassCallable(array $arguments): bool
    {
        return count($arguments) === 1
            && $arguments[0] instanceof Node\VariadicPlaceholder;
    }

    private function lowerLvalue(
        Node\Expr $expression,
        bool $allowAppend = true,
    ): array
    {
        if (
            $expression instanceof Node\Expr\Variable
            && is_string($expression->name)
        ) {
            return [
                'kind' => 'variable',
                'name' => $expression->name,
                'location' => $this->location($expression),
            ];
        }

        if ($expression instanceof Node\Expr\ArrayDimFetch) {
            if ($expression->dim === null && !$allowAppend) {
                return $this->unsupported($expression, 'array append read');
            }

            return [
                'kind' => 'array_element',
                'array' => $this->lowerLvalue($expression->var, $allowAppend),
                'key' => $expression->dim === null
                    ? null
                    : $this->lowerExpression($expression->dim),
                'location' => $this->location($expression),
            ];
        }

        if ($expression instanceof Node\Expr\PropertyFetch) {
            if (!($expression->name instanceof Node\Identifier)) {
                return $this->unsupported($expression->name, 'dynamic property');
            }

            return [
                'kind' => 'object_property',
                'object' => $this->lowerLvalue($expression->var, $allowAppend),
                'name' => $expression->name->toString(),
                'location' => $this->location($expression),
            ];
        }

        if ($expression instanceof Node\Expr\StaticPropertyFetch) {
            if (
                !($expression->class instanceof Node\Name)
                || !($expression->name instanceof Node\VarLikeIdentifier)
            ) {
                return $this->unsupported($expression, 'dynamic static property');
            }

            return [
                'kind' => 'static_property',
                'class' => $this->resolvedName($expression->class),
                'name' => $expression->name->toString(),
                'location' => $this->location($expression),
            ];
        }

        return $this->unsupported($expression, 'assignment target');
    }

    private function lowerBinaryOperator(Node\Expr\BinaryOp $operator): string
    {
        return match (true) {
            $operator instanceof Node\Expr\BinaryOp\Plus => 'add',
            $operator instanceof Node\Expr\BinaryOp\Minus => 'subtract',
            $operator instanceof Node\Expr\BinaryOp\Mul => 'multiply',
            $operator instanceof Node\Expr\BinaryOp\Div => 'divide',
            $operator instanceof Node\Expr\BinaryOp\Concat => 'concat',
            $operator instanceof Node\Expr\BinaryOp\Identical => 'identical',
            $operator instanceof Node\Expr\BinaryOp\NotIdentical => 'not_identical',
            $operator instanceof Node\Expr\BinaryOp\Equal => 'equal',
            $operator instanceof Node\Expr\BinaryOp\NotEqual => 'not_equal',
            $operator instanceof Node\Expr\BinaryOp\Smaller => 'less_than',
            $operator instanceof Node\Expr\BinaryOp\SmallerOrEqual => 'less_than_or_equal',
            $operator instanceof Node\Expr\BinaryOp\Greater => 'greater_than',
            $operator instanceof Node\Expr\BinaryOp\GreaterOrEqual => 'greater_than_or_equal',
            $operator instanceof Node\Expr\BinaryOp\BooleanAnd,
            $operator instanceof Node\Expr\BinaryOp\LogicalAnd => 'boolean_and',
            $operator instanceof Node\Expr\BinaryOp\BooleanOr,
            $operator instanceof Node\Expr\BinaryOp\LogicalOr => 'boolean_or',
            default => $this->unsupported($operator, 'binary operator'),
        };
    }

    private function unsupported(Node $node, string $description): never
    {
        $location = $this->location($node);
        throw new LoweringError(sprintf(
            '%s:%d:%d: unsupported %s (%s)',
            $this->filename,
            $location['start']['line'],
            $location['start']['column'],
            $description,
            $node->getType(),
        ));
    }

    private function location(Node $node): array
    {
        $startOffset = $node->getStartFilePos();
        $endOffset = $node->getEndFilePos();
        if ($startOffset < 0 || $endOffset < $startOffset) {
            throw new LogicException('PHP-Parser did not provide source offsets');
        }

        return [
            'file' => $this->filename,
            'start' => $this->position($startOffset),
            'end' => $this->position($endOffset + 1),
        ];
    }

    private function position(int $offset): array
    {
        if ($offset < 0 || $offset > strlen($this->source)) {
            throw new LogicException('Invalid source position from PHP-Parser');
        }

        $low = 0;
        $high = count($this->lineStarts) - 1;
        while ($low <= $high) {
            $middle = intdiv($low + $high, 2);
            if ($this->lineStarts[$middle] <= $offset) {
                $low = $middle + 1;
            } else {
                $high = $middle - 1;
            }
        }

        $lineIndex = $high;
        $lineStart = $this->lineStarts[$lineIndex];
        return [
            'line' => $lineIndex + 1,
            'column' => $offset - $lineStart + 1,
            'offset' => $offset,
        ];
    }
}

final class ProjectLowerer
{
    /** @var array<string, array> */
    private array $programs = [];

    /** @var array<string, true> */
    private array $includedFiles = [];

    /** @var array<string, true> */
    private array $composerAutoloaders = [];

    /** @var list<string> */
    private array $sourceFiles = [];

    /** @var list<array> */
    private array $functions = [];

    /** @var list<array> */
    private array $classes = [];

    /** @var array<string, true> */
    private array $declaredClasses = [];

    /** @var list<array{prefix: string, directory: string}> */
    private array $psr4 = [];

    /** @var array<string, string> */
    private array $classmap = [];

    private ?string $rootDirectory = null;

    private ?string $rootDisplayDirectory = null;

    public function __construct(private readonly PhpParser\Parser $parser)
    {
    }

    public function lower(string $filename): array
    {
        $identity = realpath($filename);
        if ($identity !== false) {
            $this->includedFiles[$identity] = true;
            $this->rootDirectory = dirname($identity);
            $this->rootDisplayDirectory = dirname($filename);
        }
        $root = $this->parseFile($filename, $filename);
        $this->mergeDeclarations($root);
        $root['statements'] = $this->expandBootstrap($root, true);
        $this->autoloadReferencedClasses($root);
        $root['source_files'] = $this->sourceFiles;
        $root['functions'] = $this->functions;
        $root['classes'] = $this->classes;
        return $root;
    }

    private function parseFile(string $filename, ?string $displayName = null): array
    {
        $identity = realpath($filename);
        if ($identity === false || !is_file($identity)) {
            throw new LoweringError(sprintf('%s: unable to read source file', $filename));
        }
        if (array_key_exists($identity, $this->programs)) {
            return $this->programs[$identity];
        }
        $displayName ??= $this->displayPath($identity);
        $source = @file_get_contents($identity);
        if ($source === false) {
            throw new LoweringError(sprintf('%s: unable to read source file', $displayName));
        }
        try {
            $statements = $this->parser->parse($source) ?? [];
        } catch (Error $error) {
            $position = $error->hasColumnInfo()
                ? sprintf('%d:%d', $error->getStartLine(), $error->getStartColumn($source))
                : (string) $error->getStartLine();
            throw new LoweringError(sprintf(
                '%s:%s: parse error: %s',
                $displayName,
                $position,
                $error->getRawMessage(),
            ));
        }
        $traverser = new NodeTraverser();
        $traverser->addVisitor(new NameResolver());
        $statements = $traverser->traverse($statements);
        $program = (new Lowerer($displayName, $source))->lowerProgram($statements);
        $this->programs[$identity] = $program;
        $this->sourceFiles[] = $displayName;
        return $program;
    }

    /** @return list<array> */
    private function expandBootstrap(array $program, bool $root): array
    {
        $body = [];
        $executableSeen = false;
        foreach ($program['statements'] as $statement) {
            if ($statement['kind'] === '__include') {
                if ($executableSeen) {
                    $this->unsupportedAt(
                        $statement['location'],
                        'include after executable top-level code',
                    );
                }
                $this->loadInclude($statement);
            } elseif ($statement['kind'] === 'nop') {
                if ($root) {
                    $body[] = $statement;
                }
            } elseif ($root) {
                $executableSeen = true;
                $body[] = $statement;
            } else {
                $this->unsupportedAt(
                    $statement['location'],
                    'executable code in an included or autoloaded file',
                );
            }
        }
        return $body;
    }

    private function loadInclude(array $include): void
    {
        $identity = realpath($include['path']);
        if ($identity === false || !is_file($identity)) {
            $this->unsupportedAt($include['location'], 'missing included file');
        }
        $once = str_ends_with($include['type'], '_once');
        if (array_key_exists($identity, $this->includedFiles)) {
            if ($once) {
                return;
            }
            $this->unsupportedAt($include['location'], 'repeated include without _once');
        }
        $this->includedFiles[$identity] = true;
        if ($this->isComposerAutoloader($identity)) {
            $this->loadComposerAutoloader($identity);
            return;
        }
        $program = $this->parseFile($identity);
        $this->mergeDeclarations($program);
        $this->expandBootstrap($program, false);
    }

    private function isComposerAutoloader(string $filename): bool
    {
        $vendor = dirname($filename);
        return basename($filename) === 'autoload.php'
            && basename($vendor) === 'vendor'
            && is_file($vendor . '/composer/installed.json');
    }

    private function loadComposerAutoloader(string $filename): void
    {
        if (array_key_exists($filename, $this->composerAutoloaders)) {
            return;
        }
        $this->composerAutoloaders[$filename] = true;
        $vendor = dirname($filename);
        $base = dirname($vendor);
        $rootManifest = $base . '/composer.json';
        if (is_file($rootManifest)) {
            $manifest = $this->readJsonObject($rootManifest);
            $this->registerAutoload($manifest['autoload'] ?? [], $base);
            $this->registerAutoload($manifest['autoload-dev'] ?? [], $base);
        }
        $installedFile = $vendor . '/composer/installed.json';
        $installed = $this->readJsonObject($installedFile);
        foreach ($installed['packages'] ?? [] as $package) {
            if (!is_array($package) || !isset($package['install-path'])) {
                continue;
            }
            $packageBase = $vendor . '/composer/' . $package['install-path'];
            $this->registerAutoload($package['autoload'] ?? [], $packageBase);
        }
    }

    private function readJsonObject(string $filename): array
    {
        $contents = @file_get_contents($filename);
        if ($contents === false) {
            throw new LoweringError(sprintf('%s: unable to read Composer metadata', $filename));
        }
        try {
            $value = json_decode($contents, true, flags: JSON_THROW_ON_ERROR);
        } catch (JsonException $error) {
            throw new LoweringError(sprintf(
                '%s: invalid Composer metadata: %s',
                $filename,
                $error->getMessage(),
            ));
        }
        if (!is_array($value)) {
            throw new LoweringError(sprintf('%s: invalid Composer metadata', $filename));
        }
        return $value;
    }

    private function registerAutoload(mixed $autoload, string $base): void
    {
        if (!is_array($autoload)) {
            return;
        }
        foreach ($autoload['psr-4'] ?? [] as $prefix => $directories) {
            foreach ((array) $directories as $directory) {
                if (is_string($directory)) {
                    $this->psr4[] = [
                        'prefix' => $prefix,
                        'directory' => $base . '/' . $directory,
                    ];
                }
            }
        }
        foreach ((array) ($autoload['classmap'] ?? []) as $path) {
            if (is_string($path)) {
                $this->indexClassmapPath($base . '/' . $path);
            }
        }
        foreach ((array) ($autoload['files'] ?? []) as $path) {
            if (!is_string($path)) {
                continue;
            }
            $identity = realpath($base . '/' . $path);
            if ($identity === false || !is_file($identity)) {
                throw new LoweringError(sprintf('%s/%s: Composer autoload file not found', $base, $path));
            }
            if (!array_key_exists($identity, $this->includedFiles)) {
                $this->includedFiles[$identity] = true;
                $program = $this->parseFile($identity);
                $this->mergeDeclarations($program);
                $this->expandBootstrap($program, false);
            }
        }
    }

    private function indexClassmapPath(string $path): void
    {
        $identity = realpath($path);
        if ($identity === false) {
            return;
        }
        if (is_file($identity)) {
            $this->indexClassmapFile($identity);
            return;
        }
        if (!is_dir($identity)) {
            return;
        }
        $files = [];
        $iterator = new RecursiveIteratorIterator(
            new RecursiveDirectoryIterator($identity, FilesystemIterator::SKIP_DOTS),
        );
        foreach ($iterator as $file) {
            if ($file->isFile() && strtolower($file->getExtension()) === 'php') {
                $files[] = $file->getPathname();
            }
        }
        sort($files, SORT_STRING);
        foreach ($files as $file) {
            $this->indexClassmapFile($file);
        }
    }

    private function indexClassmapFile(string $filename): void
    {
        $source = @file_get_contents($filename);
        if ($source === false) {
            return;
        }
        try {
            $statements = $this->parser->parse($source) ?? [];
        } catch (Error) {
            return;
        }
        $traverser = new NodeTraverser();
        $traverser->addVisitor(new NameResolver());
        $statements = $traverser->traverse($statements);
        foreach ($this->classNames($statements) as $className) {
            $this->classmap[strtolower($className)] = $filename;
        }
    }

    /**
     * @param list<Node\Stmt> $statements
     * @return list<string>
     */
    private function classNames(array $statements): array
    {
        $names = [];
        foreach ($statements as $statement) {
            if ($statement instanceof Node\Stmt\Namespace_) {
                array_push($names, ...$this->classNames($statement->stmts));
            } elseif (
                $statement instanceof Node\Stmt\ClassLike
                && $statement->name !== null
            ) {
                $name = $statement->namespacedName instanceof Node\Name
                    ? $statement->namespacedName->toString()
                    : $statement->name->toString();
                $names[] = $name;
            }
        }
        return $names;
    }

    private function autoloadReferencedClasses(array $root): void
    {
        $examined = [];
        while (true) {
            $references = [];
            $this->collectClassReferences(
                [
                    'functions' => $this->functions,
                    'classes' => $this->classes,
                    'statements' => $root['statements'],
                ],
                $references,
            );
            $loaded = false;
            foreach (array_keys($references) as $className) {
                $canonical = strtolower($className);
                if (
                    isset($examined[$canonical])
                    || isset($this->declaredClasses[$canonical])
                    || in_array($canonical, ['self', 'parent', 'static'], true)
                ) {
                    continue;
                }
                $examined[$canonical] = true;
                $filename = $this->autoloadFile($className);
                if ($filename === null) {
                    continue;
                }
                $program = $this->parseFile($filename);
                $this->mergeDeclarations($program);
                $this->expandBootstrap($program, false);
                $loaded = true;
            }
            if (!$loaded) {
                return;
            }
        }
    }

    private function autoloadFile(string $className): ?string
    {
        $canonical = strtolower($className);
        if (isset($this->classmap[$canonical])) {
            return $this->classmap[$canonical];
        }
        $mappings = $this->psr4;
        usort(
            $mappings,
            fn (array $left, array $right): int => strlen($right['prefix']) <=> strlen($left['prefix']),
        );
        foreach ($mappings as $mapping) {
            if (!str_starts_with($className, $mapping['prefix'])) {
                continue;
            }
            $relative = substr($className, strlen($mapping['prefix']));
            $candidate = $mapping['directory'] . '/' . str_replace('\\', '/', $relative) . '.php';
            $identity = realpath($candidate);
            if ($identity !== false && is_file($identity)) {
                return $identity;
            }
        }
        return null;
    }

    private function collectClassReferences(mixed $value, array &$references, ?string $key = null): void
    {
        if (is_string($value)) {
            if (in_array($key, ['class', 'parent', 'trait'], true)) {
                $references[$value] = true;
            }
            return;
        }
        if (!is_array($value)) {
            return;
        }
        if (in_array($key, ['interfaces', 'traits', 'types', 'instead_of'], true)) {
            foreach ($value as $item) {
                if (is_string($item)) {
                    $references[$item] = true;
                } else {
                    $this->collectClassReferences($item, $references);
                }
            }
            return;
        }
        foreach ($value as $childKey => $child) {
            $this->collectClassReferences(
                $child,
                $references,
                is_string($childKey) ? $childKey : null,
            );
        }
    }

    private function mergeDeclarations(array $program): void
    {
        array_push($this->functions, ...$program['functions']);
        foreach ($program['classes'] as $class) {
            $this->declaredClasses[strtolower($class['name'])] = true;
            $this->classes[] = $class;
        }
    }

    private function displayPath(string $filename): string
    {
        if (
            $this->rootDirectory !== null
            && $this->rootDisplayDirectory !== null
            && str_starts_with($filename, $this->rootDirectory . DIRECTORY_SEPARATOR)
        ) {
            $relative = substr($filename, strlen($this->rootDirectory) + 1);
            return $this->rootDisplayDirectory === '.'
                ? $relative
                : $this->rootDisplayDirectory . DIRECTORY_SEPARATOR . $relative;
        }
        $workingDirectory = realpath(getcwd());
        if (
            $workingDirectory !== false
            && str_starts_with($filename, $workingDirectory . DIRECTORY_SEPARATOR)
        ) {
            return substr($filename, strlen($workingDirectory) + 1);
        }
        return $filename;
    }

    private function unsupportedAt(array $location, string $description): never
    {
        throw new LoweringError(sprintf(
            '%s:%d:%d: unsupported %s',
            $location['file'],
            $location['start']['line'],
            $location['start']['column'],
            $description,
        ));
    }
}

function fail(string $message, int $exitCode = 2): never
{
    fwrite(STDERR, $message . PHP_EOL);
    exit($exitCode);
}

if ($argc !== 2) {
    fail('usage: lower.php FILE');
}

if (PHP_INT_SIZE !== 8) {
    fail('the Soteria PHP frontend requires a 64-bit PHP runtime');
}

$autoload = null;
foreach ([
    dirname(__DIR__) . '/vendor/autoload.php',
    __DIR__ . '/vendor/autoload.php',
] as $candidate) {
    if (is_file($candidate)) {
        $autoload = $candidate;
        break;
    }
}
if ($autoload === null) {
    fail(
        'Soteria PHP frontend dependencies are missing; run '
        . '`composer install --working-dir=soteria-php/frontend`.',
    );
}
require $autoload;

$version = array_map('intval', explode('.', TARGET_PHP_VERSION));
$parser = (new ParserFactory())->createForVersion(
    PhpVersion::fromComponents($version[0], $version[1]),
);

try {
    $ir = (new ProjectLowerer($parser))->lower($argv[1]);
    $json = json_encode(
        $ir,
        JSON_PRETTY_PRINT
            | JSON_UNESCAPED_SLASHES
            | JSON_UNESCAPED_UNICODE
            | JSON_PRESERVE_ZERO_FRACTION
            | JSON_THROW_ON_ERROR,
    );
    fwrite(STDOUT, $json . PHP_EOL);
} catch (LoweringError $error) {
    fail($error->getMessage());
} catch (Throwable $error) {
    fail('internal frontend error: ' . $error->getMessage(), 3);
}
