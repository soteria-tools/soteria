#!/usr/bin/env fish

set err_counter 0

# The json_dump directory contains files with the name cc1.json, cc2.json, etc.
# COUNTER is the first number that is not used in the json_dump directory.

mkdir -p json_dump

set COUNTER 0
while test -e json_dump/cc$COUNTER.json
    set COUNTER (math $COUNTER + 1)
end

OCAML_LANDMARKS=on opam exec -- dune exec --instrument-with=landmarks -- soteria-c gen-summaries \
       ../Collections-C/src/cc_array.c ../Collections-C/src/cc_common.c \
       ../Collections-C/src/cc_deque.c ../Collections-C/src/cc_hashset.c \
       ../Collections-C/src/cc_list.c \
       ../Collections-C/src/cc_hashtable.c \
       ../Collections-C/src/cc_pqueue.c ../Collections-C/src/cc_queue.c \
       ../Collections-C/src/cc_ring_buffer.c ../Collections-C/src/cc_slist.c \
       ../Collections-C/src/cc_stack.c ../Collections-C/src/cc_treeset.c \
       ../Collections-C/src/cc_treetable.c ../Collections-C/src/cc_tsttable.c \
       -I ../Collections-C/src/include/ \
       --dump-unsupported json_dump/dump.json \
       --solver-timeout 250
       > json_dump/out.summaries


echo "Finding manifest bugs in summaries..."
set bug_count (grep -c "manifest bugs:" json_dump/out.summaries | xargs)
set empty_bug_count (grep -c "manifest bugs: \[\]" json_dump/out.summaries | xargs)
set real_bug_count (math $bug_count - $empty_bug_count)

if test $real_bug_count -gt 0
       echo -e "Found \033[1;32m$real_bug_count lines\033[0m with non-empty manifest bugs"
else
       echo "Found $real_bug_count lines with non-empty manifest bugs"
end


set TO_REMOVE "MISSING FEATURE, VANISHING: Could not resolve function"
jq "to_entries | sort_by(.value) | reverse | from_entries" json_dump/dump.json > json_dump/cc$COUNTER.json
rm -f json_dump/dump.json

set CURRENT_UNIMPL $(jq "to_entries | map(select(.key != \"$TO_REMOVE\")) | map(.value) | add" json_dump/cc$COUNTER.json)

echo "remaining unimplemented branches: $CURRENT_UNIMPL"

if test -e json_dump/cc(math $COUNTER - 1).json
       set PREVIOUS_UNIMPL $(jq "to_entries | map(select(.key != \"$TO_REMOVE\")) | map(.value) | add" json_dump/cc(math $COUNTER - 1).json)
       if test $CURRENT_UNIMPL -lt $PREVIOUS_UNIMPL
              set DECREASE (math $PREVIOUS_UNIMPL - $CURRENT_UNIMPL)
              echo -e "That is \033[32m$DECREASE less\033[0m than the previous run"
       else if test $CURRENT_UNIMPL -eq $PREVIOUS_UNIMPL
              echo -e "\033[33mNothing has changed\033[0m"
       else
              set INCREASE (math $CURRENT_UNIMPL - $PREVIOUS_UNIMPL)
              echo -e "That is \033[31m$INCREASE more\033[0m than the previous run"
       end
end