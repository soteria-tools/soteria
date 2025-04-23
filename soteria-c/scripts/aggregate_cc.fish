#!/usr/bin/env fish

set err_counter 0



rm -rf json_dump
mkdir -p json_dump
OCAML_LANDMARKS=on opam exec -- dune exec --instrument-with landmarks -- soteria-c gen-summaries \
       ../Collections-C/src/cc_array.c ../Collections-C/src/cc_common.c \
       ../Collections-C/src/cc_deque.c ../Collections-C/src/cc_hashset.c \
       ../Collections-C/src/cc_list.c \
       # ../Collections-C/src/cc_hashtable.c \ # TODO: Cerberus doesn't parse this correctly
       ../Collections-C/src/cc_pqueue.c ../Collections-C/src/cc_queue.c \
       ../Collections-C/src/cc_ring_buffer.c ../Collections-C/src/cc_slist.c \
       ../Collections-C/src/cc_stack.c ../Collections-C/src/cc_treeset.c \
       ../Collections-C/src/cc_treetable.c ../Collections-C/src/cc_tsttable.c \
       -I ../Collections-C/src/include/ \
       --dump-unsupported json_dump/dump.json \
       || set err_counter (math $err_counter + 1)




# jq -s 'reduce .[] as $obj ({};
#   reduce ($obj | keys_unsorted[]) as $key (.;
#     .[$key] = (.[$key] // 0) + $obj[$key]
#     )
#   ) | to_entries | sort_by(.value) | reverse | from_entries' json_dump/*.json > out.json

# echo "Done, $err_counter files failed"