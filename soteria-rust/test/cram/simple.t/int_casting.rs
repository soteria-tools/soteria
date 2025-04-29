
fn main() {
    // u8
    let x: u8 = 64;
    assert!(x as u8 == 64);
    assert!(x as i8 == 64);
    assert!(x as u16 == 64);
    assert!(x as i16 == 64);

    let x: u8 = 129;
    assert!(x as u8 == 129);
    assert!(x as i8 == -127);
    assert!(x as u16 == 129);
    assert!(x as i16 == 129);

    // i8
    let x: i8 = 64;
    assert!(x as u8 == 64);
    assert!(x as i8 == 64);
    assert!(x as u16 == 64);
    assert!(x as i16 == 64);

    let x: i8 = -64;
    assert!(x as u8 == 192);
    assert!(x as i8 == -64);
    assert!(x as u16 == 65472);
    assert!(x as i16 == -64);

    // u16
    let x: u16 = 64;
    assert!(x as u8 == 64);
    assert!(x as i8 == 64);
    assert!(x as u16 == 64);
    assert!(x as i16 == 64);

    let x: u16 = 33000;
    assert!(x as u8 == 232);
    assert!(x as i8 == -24);
    assert!(x as u16 == 33000);
    assert!(x as i16 == -32536);

    // i16
    let x: i16 = 640;
    assert!(x as u8 == 128);
    assert!(x as i8 == -128);
    assert!(x as u16 == 640);
    assert!(x as i16 == 640);

    let x: i16 = -32000;
    assert!(x as u8 == 0);
    assert!(x as i8 == 0);
    assert!(x as u16 == 33536);
    assert!(x as i16 == -32000);
}
