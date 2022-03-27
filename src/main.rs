use std::collections::HashMap;
use std::fmt;

fn main() {
    let world = single_nook_world(5, 5);
    println!("{}", world);
}

#[derive(Debug, Clone, PartialEq)]
struct Nook {
    weight: u8,
}

#[derive(Debug, Clone, PartialEq)]
enum Thing {
    Nook(Nook),
    Food,
}

struct World {
    nx: u128,
    ny: u128,
    things: HashMap<(u128, u128), Thing>,
}

impl World {
    fn total_weight(&self) -> u128 {
        self.things.values().fold(0, |sum, thing| {
            sum + match thing {
                Thing::Nook(nook) => nook.weight.into(),
                Thing::Food => 1,
            }
        })
    }
}

fn single_nook_world(nx: u128, ny: u128) -> World {
    let mut things = HashMap::new();
    for x in 0..nx {
        for y in 0..ny {
            let thing;
            if x == (nx - 1) / 2 && y == (ny - 1) / 2 {
                thing = Thing::Nook(Nook { weight: 1 });
            } else {
                thing = Thing::Food;
            }
            things.insert((x, y), thing);
        }
    }
    World { nx, ny, things }
}

impl fmt::Display for World {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.nx, self.ny)
    }
}

#[test]
fn test_single_nook_world() {
    let world = single_nook_world(5, 5);
    assert_eq!(
        world.things.get(&(2, 2)),
        Some(&Thing::Nook(Nook { weight: 1 }))
    );
}

#[test]
fn test_total_weight() {
    assert_eq!(single_nook_world(1, 1).total_weight(), 1);
    assert_eq!(single_nook_world(1, 2).total_weight(), 2);
    assert_eq!(single_nook_world(2, 1).total_weight(), 2);
    assert_eq!(single_nook_world(2, 2).total_weight(), 4);
    assert_eq!(single_nook_world(3, 3).total_weight(), 9);
}
