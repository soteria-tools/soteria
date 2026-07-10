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
const SCHEMA_VERSION = 1;

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
        return [
            'schema_version' => SCHEMA_VERSION,
            'target_php_version' => TARGET_PHP_VERSION,
            'source_file' => $this->filename,
            'statements' => array_map($this->lowerStatement(...), $statements),
        ];
    }

    private function lowerStatement(Node\Stmt $statement): array
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
            $statement instanceof Node\Stmt\Nop => [
                'kind' => 'nop',
                'location' => $location,
            ],
            default => $this->unsupported($statement, 'statement'),
        };
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

        return $this->unsupported($expression, 'expression');
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
