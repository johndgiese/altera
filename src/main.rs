mod modulo;
use modulo::Mod;

use std::cmp;
use std::collections::HashMap;

fn main() {
    // TODO: if file provided, load the world from it, else create world of specified size
    let mut world = single_nook_world(5, 5);
    let num_steps = 10;
    for i in 1..num_steps {
        println!("Step {}", i);
        world.print();
        world.step();
    }
    // TODO: save the final file to the specified location
}

#[derive(Debug, Clone, PartialEq)]
struct Nook {
    weight: u8,
}

type View = [[u8; 5]; 5];

type Position = (isize, isize);

impl Nook {
    fn act(&self, view: View) -> Action {
        // TODO: implement this
        Action::Rest
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Thing {
    Nook(Nook),
    Food,
}

#[derive(Debug, Clone, PartialEq)]
enum Direction {
    Up,
    Left,
    Down,
    Right,
}

#[derive(Debug, Clone, PartialEq)]
enum Action {
    Move(Direction),
    Eat(Direction),
    Split(Direction),
    Rest,
}

struct World {
    nx: isize,
    ny: isize,
    things: HashMap<Position, Thing>,
}

impl World {
    fn step(&mut self) {
        let num_actions = self.num_nooks().try_into().unwrap();
        let mut actions: Vec<Action> = Vec::with_capacity(num_actions);
        for (position, thing) in self.things.iter() {
            match thing {
                Thing::Nook(nook) => {
                    let view = self.get_view(position);
                    let action = nook.act(view);
                    actions.push(action);
                }
                _ => {}
            }
        }
        self.apply_actions(actions);
    }

    fn get_view(&self, center: &Position) -> View {
        let mut view: View = [[0u8; 5]; 5];
        let yc = center.0;
        let xc = center.1;
        for (iy, y) in (-2..=2).enumerate() {
            for (ix, x) in (-2..=2).enumerate() {
                let yp = (yc + y).modulo(self.ny);
                let xp = (xc + x).modulo(self.nx);
                view[iy][ix] = self.get_weight(&(yp, xp));
            }
        }
        view
    }

    fn get_weight(&self, p: &Position) -> u8 {
        match self.things.get(p) {
            None => 0,
            Some(Thing::Nook(nook)) => nook.weight,
            Some(Thing::Food) => 1,
        }
    }

    fn total_weight(&self) -> u128 {
        self.things.values().fold(0, |sum, thing| {
            sum + match thing {
                Thing::Nook(nook) => nook.weight.into(),
                Thing::Food => 1,
            }
        })
    }

    fn num_nooks(&self) -> u128 {
        self.things.values().fold(0, |sum, thing| {
            sum + match thing {
                Thing::Nook(_) => 1,
                Thing::Food => 0,
            }
        })
    }

    fn apply_actions(&self, actions: Vec<Action>) {
        // TODO: implement this
    }

    fn print(&self) {
        for y in 0..self.ny {
            for x in 0..self.nx {
                let position = (y, x);
                let thing = self.things.get(&position);
                match thing {
                    None => print!(" "),
                    Some(Thing::Food) => print!("."),
                    Some(Thing::Nook(nook)) => print!("{}", cmp::min(nook.weight, 9)),
                }
            }
            println!("");
        }
    }
}

fn single_nook_world(nx: isize, ny: isize) -> World {
    let mut things = HashMap::new();
    for y in 0..ny {
        for x in 0..nx {
            let thing;
            if x == (nx - 1) / 2 && y == (ny - 1) / 2 {
                thing = Thing::Nook(Nook { weight: 1 });
            } else {
                thing = Thing::Food;
            }
            things.insert((y, x), thing);
        }
    }
    World { ny, nx, things }
}

fn world_from_func<F>(ny: isize, nx: isize, func: F) -> World
where
    F: Fn(Position) -> Thing,
{
    let mut things = HashMap::new();
    for y in 0..ny {
        for x in 0..nx {
            let position = (y, x);
            things.insert(position, func(position));
        }
    }
    World { ny, nx, things }
}

#[test]
fn test_single_nook_world() {
    let world = single_nook_world(5, 5);
    assert_eq!(world.total_weight(), 25);
    assert_eq!(world.num_nooks(), 1);
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

#[test]
fn test_get_view() {
    let mut world = world_from_func(10, 10, |(_, x)| {
        Thing::Nook(Nook {
            weight: x.try_into().unwrap_or(0),
        })
    });
    assert_eq!(
        world.get_view(&(2, 2)),
        [
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
        ]
    );
    assert_eq!(
        world.get_view(&(2, 0)),
        [
            [8, 9, 0, 1, 2],
            [8, 9, 0, 1, 2],
            [8, 9, 0, 1, 2],
            [8, 9, 0, 1, 2],
            [8, 9, 0, 1, 2],
        ]
    );
}
