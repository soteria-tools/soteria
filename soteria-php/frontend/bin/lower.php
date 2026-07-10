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
const SCHEMA_VERSION = 6;

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
        $functions = [];
        $functionNames = [];
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
            } else {
                $body[] = $this->lowerStatement($statement, false, 0);
            }
        }

        return [
            'schema_version' => SCHEMA_VERSION,
            'target_php_version' => TARGET_PHP_VERSION,
            'source_file' => $this->filename,
            'functions' => $functions,
            'statements' => $body,
        ];
    }

    private function lowerFunction(Node\Stmt\Function_ $function): array
    {
        if ($function->byRef) {
            return $this->unsupported($function, 'by-reference function return');
        }
        if ($function->returnType !== null) {
            return $this->unsupported($function->returnType, 'function return type');
        }
        if ($function->attrGroups !== []) {
            return $this->unsupported($function->attrGroups[0], 'function attribute');
        }

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
            'location' => $this->location($function),
        ];
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

        if ($expression instanceof Node\Expr\FuncCall) {
            if (!($expression->name instanceof Node\Name)) {
                return $this->unsupported($expression, 'dynamic function call');
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
                'kind' => 'call',
                'name' => $this->resolvedName($expression->name),
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

$filename = $argv[1];
$source = @file_get_contents($filename);
if ($source === false) {
    fail(sprintf('%s: unable to read source file', $filename));
}

$version = array_map('intval', explode('.', TARGET_PHP_VERSION));
$parser = (new ParserFactory())->createForVersion(
    PhpVersion::fromComponents($version[0], $version[1]),
);

try {
    $statements = $parser->parse($source) ?? [];
} catch (Error $error) {
    $position = $error->hasColumnInfo()
        ? sprintf('%d:%d', $error->getStartLine(), $error->getStartColumn($source))
        : (string) $error->getStartLine();
    fail(sprintf('%s:%s: parse error: %s', $filename, $position, $error->getRawMessage()));
}

try {
    $traverser = new NodeTraverser();
    $traverser->addVisitor(new NameResolver());
    $statements = $traverser->traverse($statements);
    $ir = (new Lowerer($filename, $source))->lowerProgram($statements);
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
