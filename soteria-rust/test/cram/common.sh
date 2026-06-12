check_allocs() {
	local input_file="$1"
	local expected="$2"

	soteria-rust exec "$input_file" --stats stats.json

	local allocs
	allocs="$(jq '."soteria-rust.allocs" // 0' stats.json)"

	if [ "$allocs" != "$expected" ]; then
		echo "check_allocs: expected '$expected', got '$allocs'" >&2
		return 1
	fi
}
check_decays() {
	local input_file="$1"
	local expected="$2"

	soteria-rust exec "$input_file" --stats stats.json

	local decays
	decays="$(jq '."soteria-rust.decayed_pointers" // 0' stats.json)"

	if [ "$decays" != "$expected" ]; then
		echo "check_decays: expected '$expected', got '$decays'" >&2
		return 1
	fi
}