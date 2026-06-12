check_stat() {
	local stats_file="$1"
	local stat="$2"
	local expected="$3"

	local value
	value="$(jq ".\"soteria-rust.$stat\" // 0" "$stats_file")"

	if [ "$value" != "$expected" ]; then
		echo "check_stat: expected '$expected', got '$value' for $stat" >&2
		return 1
	fi
}
