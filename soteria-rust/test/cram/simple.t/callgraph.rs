fn inc(x: i32) -> i32 {
    x + 1
}

fn dec(x: i32) -> i32 {
    x - 1
}

fn twice(x: i32) -> i32 {
    x * 2
}

fn choose(x: i32) -> i32 {
    if x % 2 == 0 {
        twice(inc(x))
    } else {
        dec(twice(x))
    }
}

fn ping(n: i32) -> i32 {
    if n <= 0 {
        0
    } else {
        1 + pong(n - 1)
    }
}

fn pong(n: i32) -> i32 {
    if n <= 0 {
        0
    } else {
        1 + ping(n - 1)
    }
}

fn score(x: i32) -> i32 {
    choose(x) + ping(3)
}

fn limit(x: i32) -> i32 {
    if x < 0 {
        0
    } else if x > 50 {
        50
    } else {
        x
    }
}

fn run(start: i32) -> i32 {
    let mut total = 0;
    for i in 0..4 {
        total += score(start + i);
    }
    limit(total)
}

fn main() {
    let out = run(5);
    println!("result={out}");
}
