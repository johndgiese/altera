use std::fmt;

fn main() {
    let world = World { nx: 5, ny: 5 };
    println!("{}", world);
}

struct World {
    nx: u128,
    ny: u128,
}

impl fmt::Display for World {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.nx, self.ny)
    }
}
