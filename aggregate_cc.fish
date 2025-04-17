#!/usr/bin/env fish

set err_counter 0

rm -rf json_dump
mkdir -p json_dump
for file in ../Collections-C/src/*.c
  echo (basename $file .c)
  _build/install/default/bin/soteria-c gen-summaries $file -I ../Collections-C/src/include/ --dump-unsupported json_dump/(basename $file .c).json || set err_counter (math $err_counter + 1)
end


echo "Generating json"

jq -s 'reduce .[] as $obj ({};
  reduce ($obj | keys_unsorted[]) as $key (.;
    .[$key] = (.[$key] // 0) + $obj[$key]
    )
  ) | to_entries | sort_by(.value) | reverse | from_entries' json_dump/*.json > out.json

echo "Done, $err_counter files failed"