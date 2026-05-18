soteria-c exec $* --dump-stats stats.json
R=$?
echo ""
echo "Executed $(jq ".[\"soteria.steps\"]" stats.json) statements"
echo "Exit code: $R"