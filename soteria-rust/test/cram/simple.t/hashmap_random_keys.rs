use std::collections::HashMap;

fn main() {
    // Creating a HashMap internally calls std::sys::random::hashmap_random_keys
    // to get random seeds for the hasher
    let mut map1: HashMap<i32, i32> = HashMap::new();
    map1.insert(1, 100);
    // map1.insert(2, 200);

    // assert_eq!(map1.get(&1), Some(&100));
    // assert_eq!(map1.get(&2), Some(&200));

    // // Create another HashMap to verify the stub works multiple times
    // let mut map2: HashMap<&str, i32> = HashMap::new();
    // map2.insert("hello", 42);
    // map2.insert("world", 84);

    // assert_eq!(map2.get("hello"), Some(&42));
    // assert_eq!(map2.get("world"), Some(&84));

    // // Test with different key types
    // let mut map3: HashMap<u64, &str> = HashMap::new();
    // map3.insert(123, "test");

    // assert_eq!(map3.get(&123), Some(&"test"));
}
