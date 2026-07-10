<?php

declare(strict_types=1);

$ir = json_decode(
    file_get_contents($argv[1]),
    true,
    flags: JSON_THROW_ON_ERROR,
);

printf(
    "schema=%d php=%s source=%s\n",
    $ir['schema_version'],
    $ir['target_php_version'],
    $ir['source_file'],
);

foreach ($ir['statements'] as $statement) {
    $start = $statement['location']['start'];
    $end = $statement['location']['end'];
    printf(
        "%s %d:%d-%d:%d [%d,%d)\n",
        $statement['kind'],
        $start['line'],
        $start['column'],
        $end['line'],
        $end['column'],
        $start['offset'],
        $end['offset'],
    );

    foreach ($statement['expressions'] ?? [] as $expression) {
        $value = array_key_exists('value', $expression)
            ? json_encode($expression['value'], JSON_THROW_ON_ERROR)
            : '-';
        printf("  %s=%s\n", $expression['kind'], $value);
    }
}
