<?php

declare(strict_types=1);

// [versionsync: PHP_VERSION=8.4.19]
const TARGET_PHP_VERSION = '8.4.19';

function decode_float(string $value): float
{
    return match ($value) {
        'INF' => INF,
        '-INF' => -INF,
        'NAN' => NAN,
        default => (float) $value,
    };
}

function decode_value(array $value): mixed
{
    return match ($value['type']) {
        'null' => null,
        'bool' => $value['value'],
        'int' => (int) $value['value'],
        'float' => decode_float($value['value']),
        'string' => $value['value'],
        default => throw new RuntimeException('unknown input type'),
    };
}

function encode_float(float $value): string
{
    return match (true) {
        is_nan($value) => 'NAN',
        $value === INF => 'INF',
        $value === -INF => '-INF',
        default => sprintf('%.17g', $value),
    };
}

function encode_value(mixed $value): array
{
    return match (get_debug_type($value)) {
        'bool' => ['type' => 'bool', 'value' => $value],
        'int' => ['type' => 'int', 'value' => (string) $value],
        'float' => ['type' => 'float', 'value' => encode_float($value)],
        'string' => ['type' => 'string', 'value' => $value],
        default => throw new RuntimeException('unexpected result type'),
    };
}

function cast_value(string $target, mixed $value): mixed
{
    return match ($target) {
        'bool' => (bool) $value,
        'int' => (int) $value,
        'float' => (float) $value,
        'string' => (string) $value,
        default => throw new RuntimeException('unknown coercion target'),
    };
}

function observe_cast(string $target, mixed $value): array
{
    $warnings = [];
    set_error_handler(
        static function (int $severity, string $message) use (&$warnings): bool {
            $warnings[] = ['severity' => $severity, 'message' => $message];
            return true;
        },
    );

    try {
        $result = ['value' => encode_value(cast_value($target, $value))];
    } catch (Throwable $error) {
        $result = [
            'error' => [
                'class' => get_class($error),
                'message' => $error->getMessage(),
            ],
        ];
    } finally {
        restore_error_handler();
    }

    $result['warnings'] = $warnings;
    return $result;
}

if (PHP_VERSION !== TARGET_PHP_VERSION) {
    fwrite(STDERR, sprintf("expected PHP %s, found %s\n", TARGET_PHP_VERSION, PHP_VERSION));
    exit(2);
}

if (PHP_INT_SIZE !== 8) {
    fwrite(STDERR, "the PHP coercion oracle requires 64-bit integers\n");
    exit(2);
}

ini_set('precision', '14');

$cases = json_decode(file_get_contents($argv[1]), true, flags: JSON_THROW_ON_ERROR);
$targets = ['bool', 'int', 'float', 'string'];
$results = [];

foreach ($cases as $case) {
    $value = decode_value($case);
    $casts = [];
    foreach ($targets as $target) {
        $casts[$target] = observe_cast($target, $value);
    }
    $results[] = ['input' => $case, 'casts' => $casts];
}

echo json_encode(
    ['php_version' => PHP_VERSION, 'results' => $results],
    JSON_THROW_ON_ERROR | JSON_UNESCAPED_SLASHES,
), "\n";
