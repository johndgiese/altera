use std::collections::HashMap;

fn main() {
    // if file provided, load the world from it
    // else create a single nook world
    let mut world = single_nook_world(5, 5);
    // step for the specified number of steps, printing after each one
    world.print();
    world.step();
    world.print();
    // save the final file to the specified location
}

#[derive(Debug, Clone, PartialEq)]
struct Nook {
    weight: u8,
}

type View = [[u8; 5]; 5];

type Position = (usize, usize);

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
    nx: usize,
    ny: usize,
    things: HashMap<Position, Thing>,
}

impl World {
    fn step(&mut self) {
        let mut actions: Vec<Action> = Vec::with_capacity(self.num_nooks());
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

    fn get_view(&self, p: &Position) -> View {
        // TODO: implement this
        [
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
        ]
    }

    fn total_weight(&self) -> usize {
        self.things.values().fold(0, |sum, thing| {
            sum + match thing {
                Thing::Nook(nook) => nook.weight.into(),
                Thing::Food => 1,
            }
        })
    }

    fn num_nooks(&self) -> usize {
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
        for x in 0..self.nx {
            for y in 0..self.ny {
                let position = (x, y);
                let thing = self.things.get(&position);
                match thing {
                    None => print!("."),
                    Some(Thing::Food) => print!("f"),
                    Some(Thing::Nook(_)) => print!("n"),
                }
            }
            println!("");
        }
    }
}

fn single_nook_world(nx: usize, ny: usize) -> World {
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
