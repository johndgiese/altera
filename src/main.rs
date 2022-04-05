//! Altera is a commandline tool for running a simulated two-dimensional grid
//! [World] that wraps around on itself. Each square of the grid may be empty,
//! have a piece of food, or a [Nook]. Nooks are little creatures that have a
//! weight between 1 and 255. The world progresses forward in discreete time
//! steps. Food is stationary and never moves. At each point in time nooks is
//! provided a small [View] of the contents of the grid surrounding themselves
//! and, based on this view, they choose an [Action]: move to an adjacent
//! square, eat the contents of an adjacent spot, split in two (i.e., asexual
//! reproduction), or sit still. Nooks may eat other nooks that don't weigh more
//! than themselves. When they eat food, or a nook, they gain the weight of the
//! object they ate. If ever a nook's weight goes above 255 the remaining weight
//! is distributed randomly onto empty squares in the world. Thus, there is a
//! law of "conservation of weight". Periodically, every nook in the world
//! looses one weight. The weight that remains is, likewise, distributed across
//! the world randomly. If a nook's weight drops to 0, they die. If all of the
//! nook's die the world will reach a static state.
//!
//! Each nook will eventually have some internal decision-making system that,
//! given a [View] selects an [Action]. This system may also pass state between
//! calls to itself, opening the door for some sort of memory.
//!
//! When Nook's reproduce they split their weight in two (the "child"'s weight
//! is rounded down if the parent's weight is an odd number). The child's
//! decision-making system is also copied as is its state, but the copy that is
//! made is not perfect; small mutations may be made.
//!
//! Alter has a couple of design goals:
//!
//! 1. It must be deterministic; the same [World] in the same state must unfold
//! in the same way given the same version of the code.
//!
//! 2. It must be possible to calculate each nook's next action in parallel from
//! the other Nooks. This is important since it will allow the most
//! computationally intensive code to be run in parallel.
//!
//! To accomplish both of these goals, each nook calculates its next [Action]
//! using a [View] taken from the state of the grid at the end of the previous
//! time step. After all of the actions are selected, each action is applied in
//! a particular order. Thus, the state of the world at the time when the action
//! is applied may be different than the state when that nook made the decision.
//! Clever nooks will need to take this oddity in their laws of physics into
//! account when making their decisions.
mod modulo;
use modulo::Mod;
mod linked_list;
use linked_list::LinkedList;

use std::cmp;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::vec::Vec;

fn main() {
    // TODO: if file provided, load the world from it, else create world of specified size
    // TODO: add random seed
    let mut world = single_nook_world(5, 5);
    let num_steps = 10;
    for i in 1..num_steps {
        println!("Step {}", i);
        world.print();
        world.step();
    }
    // TODO: save the final file to the specified location
}

// TODO: use Trait Objects to allow Nooks with different implementations all to run at the same time
#[derive(Debug, Clone, PartialEq)]
struct Nook {
    weight: u8,
    // TODO: add the internal state of the Nook which is passed between time steps
}

type View = [[u8; 5]; 5];

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Position {
    y: isize,
    x: isize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.y, self.x)
    }
}

impl Nook {
    fn act(&self, view: View) -> Action {
        // TODO: implement this
        Action::Rest
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Thing {
    Nook(Rc<Nook>),
    Food,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Direction {
    Up,
    Left,
    Down,
    Right,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Action {
    Move(Direction),
    Eat(Direction),
    Split(Direction),
    Rest,
}

/// A World stores the complete state of a particular simulation instance. It
/// includes two data structures that have some redundant information:
///
/// 1. [Nook::map] keeps track of which squares have food or nooks on them
///
/// 2. [Nook::nooks] is a linked list that tracks the order in which the nooks
/// were created; the order is important since the nook's actions must be
/// applied in that order.
///
/// Both data structures share ownership of the nooks and both contain copies of
/// the nook's current position. Thus, when a nook moves, a nook is added, or a
/// nook is removed, both data structures need to be updated together.
struct World {
    // TODO: add the random number generator's state
    ny: isize,
    nx: isize,
    map: HashMap<Position, Thing>,
    nooks: LinkedList<(Position, Action, Rc<Nook>)>,
}

impl World {
    fn step(&mut self) {
        // Collect all Nook's actions given the current state of the world;
        // then process them. Nook actions are all calculated upfront in order
        // to allow the process to be parallelizable.
        let mut actions: Vec<Action> = Vec::with_capacity(self.nooks.len());
        for (i, (position, _, nook)) in self.nooks.iter().enumerate() {
            let view = self.get_view(&position);
            actions[i] = nook.act(view);
        }
        for (i, node) in self.nooks.iter_mut().enumerate() {
            node.1 = actions[i];
        }
        self.apply_actions();
    }

    fn add_nook(&mut self, position: &Position, nook: Rc<Nook>) {
        match self.map.get(position) {
            None => {
                self.nooks
                    .push_back((position.clone(), Action::Rest, Rc::clone(&nook)));
                self.map
                    .insert(position.clone(), Thing::Nook(Rc::clone(&nook)));
            }
            _ => panic!("Thing already at {}", position),
        }
    }

    fn add_food(&mut self, position: &Position) {
        match self.map.get(position) {
            None => {
                self.map.insert(position.clone(), Thing::Food);
            }
            Some(_) => panic!("Thing already at {}", position),
        }
    }

    /// Remove nook at specified location from the world, panicing if there
    /// isn't a nook there.  Removes references in `self.map` and `self.nooks`.
    fn remove_nook(&mut self, position: &Position) {
        match self.map.remove(position) {
            Some(Thing::Nook(nook)) => self.remove_nook_from_list(nook),
            Some(Thing::Food) => panic!("Expected nook at {}, found food", position),
            None => panic!("Expected nook at {}, found nothing", position),
        }
    }

    fn remove_nook_from_list(&mut self, nook: Rc<Nook>) {
        let mut cursor = self.nooks.cursor();
        while match cursor.next() {
            None => panic!("Nook from world.map not found in world.nooks"),
            Some((_, _, other)) if Rc::ptr_eq(&nook, other) => false,
            _ => true,
        } {}
        cursor.remove_prev().unwrap();
    }

    fn get_view(&self, center: &Position) -> View {
        let mut view: View = [[0u8; 5]; 5];
        let yc = center.y;
        let xc = center.x;
        for (iy, y) in (-2..=2).enumerate() {
            for (ix, x) in (-2..=2).enumerate() {
                let yp = (yc + y).modulo(self.ny);
                let xp = (xc + x).modulo(self.nx);
                let center_modulo = Position { y: yp, x: xp };
                view[iy][ix] = self.get_weight(&center_modulo);
            }
        }
        view
    }

    fn get_weight(&self, p: &Position) -> u8 {
        match self.map.get(p) {
            None => 0,
            Some(Thing::Nook(nook)) => nook.weight,
            Some(Thing::Food) => 1,
        }
    }

    fn total_weight(&self) -> u128 {
        self.map.values().fold(0, |sum, thing| {
            sum + match thing {
                Thing::Nook(nook) => nook.weight.into(),
                Thing::Food => 1,
            }
        })
    }

    fn num_nooks(&self) -> u128 {
        self.map.values().fold(0, |sum, thing| {
            sum + match thing {
                Thing::Nook(_) => 1,
                Thing::Food => 0,
            }
        })
    }

    fn move_to(&self, start: &Position, direction: &Direction) -> Position {
        match direction {
            Direction::Down => Position {
                y: (start.y - 1).modulo(self.ny),
                x: start.x,
            },
            Direction::Up => Position {
                y: (start.y + 1).modulo(self.ny),
                x: start.x,
            },
            Direction::Left => Position {
                y: start.y,
                x: (start.x - 1).modulo(self.nx),
            },
            Direction::Right => Position {
                y: start.y,
                x: (start.x + 1).modulo(self.nx),
            },
        }
    }

    fn apply_actions(&mut self) {
        let mut cursor = self.nooks.cursor();
        while let Some((position, action, nook)) = cursor.next() {
            match action {
                Rest => (),
                Action::Move(direction) => {
                    let new_position = self.move_to(position, direction);
                    match self.map.get(&new_position) {
                        None => {
                            self.map.remove(position);
                            // move
                        }
                        Some(Thing::Food) => {}
                        Some(Thing::Nook(other)) => {}
                    }
                    // TODO: handle
                    //   see what's on the square
                    //   if there's something there, skip turn (TODO: implement pushing)
                    //   if there's nothing there, move the nook
                }
                Action::Split(direction) => {
                    // TODO: handle
                    //   see what's on the square
                    //   if there's nothing there, create a new Nook with 1/2 the weight rounding down
                }
                Action::Eat(direction) => {
                    // TODO: handle
                    //   see what's on the square
                    //   if it's food, increase weight
                    //   if it's a nook and it weights less, eat the nook
                }
            }
        }
        // TODO: handle starvation
        // TODO: add back weight that is removed from Nooks
    }

    fn print(&self) {
        for y in 0..self.ny {
            for x in 0..self.nx {
                let position = Position { y, x };
                let thing = self.map.get(&position);
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

fn single_nook_world(ny: isize, nx: isize) -> World {
    let mut world = blank_world(ny, nx);
    for y in 0..ny {
        for x in 0..nx {
            let position = Position { y, x };
            if x == (nx - 1) / 2 && y == (ny - 1) / 2 {
                let nook = Rc::new(Nook { weight: 1 });
                world.add_nook(&position, nook);
            } else {
                world.add_food(&position);
            }
        }
    }
    world
}

fn world_from_func<F>(ny: isize, nx: isize, func: F) -> World
where
    F: Fn(Position) -> Thing,
{
    let mut world = blank_world(ny, nx);
    for y in 0..ny {
        for x in 0..nx {
            let position = Position { y, x };
            match func(position) {
                Thing::Food => world.add_food(&position),
                Thing::Nook(nook) => world.add_nook(&position, nook),
            }
        }
    }
    world
}

fn blank_world(ny: isize, nx: isize) -> World {
    let mut map = HashMap::new();
    let mut nooks = LinkedList::new();
    let mut world = World { ny, nx, map, nooks };
    world
}

#[test]
fn test_single_nook_world() {
    let world = single_nook_world(5, 5);
    assert_eq!(world.total_weight(), 25);
    assert_eq!(world.num_nooks(), 1);
    let nook = Rc::new(Nook { weight: 1 });
    let position = Position { y: 2, x: 2 };
    assert_eq!(world.map.get(&position), Some(&Thing::Nook(nook)));
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
    let mut world = world_from_func(10, 10, |p| {
        let nook = Rc::new(Nook {
            weight: p.x.try_into().unwrap_or(0),
        });
        Thing::Nook(nook)
    });
    assert_eq!(
        world.get_view(&Position { y: 2, x: 2 }),
        [
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
        ]
    );
    assert_eq!(
        world.get_view(&Position { y: 2, x: 0 }),
        [
            [8, 9, 0, 1, 2],
            [8, 9, 0, 1, 2],
            [8, 9, 0, 1, 2],
            [8, 9, 0, 1, 2],
            [8, 9, 0, 1, 2],
        ]
    );
}

#[test]
fn test_add_and_remove_nooks() {
    let mut world = blank_world(3, 3);
    let nook = Rc::new(Nook { weight: 1 });
    assert_eq!(world.nooks.len(), 0);
    world.add_nook(&Position { y: 0, x: 0 }, nook);
    assert_eq!(world.nooks.len(), 1);
    world.remove_nook(&Position { y: 0, x: 0 });
    assert_eq!(world.nooks.len(), 0);
}

#[test]
fn test_move_to() {
    let ny = 4;
    let nx = 3;
    let world = blank_world(ny, nx);
    assert_eq!(
        world.move_to(&Position { y: 0, x: 0 }, &Direction::Up),
        Position { y: ny - 1, x: 0 }
    );
    assert_eq!(
        world.move_to(&Position { y: 0, x: 0 }, &Direction::Down),
        Position { y: 1, x: 0 }
    );
    assert_eq!(
        world.move_to(&Position { y: 0, x: 0 }, &Direction::Left),
        Position { y: 0, x: nx - 1 }
    );
    assert_eq!(
        world.move_to(&Position { y: 0, x: 0 }, &Direction::Right),
        Position { y: 0, x: 1 }
    );
    assert_eq!(
        world.move_to(
            &Position {
                y: ny - 1,
                x: nx - 1
            },
            &Direction::Up
        ),
        Position {
            y: ny - 2,
            x: nx - 1
        }
    );
    assert_eq!(
        world.move_to(
            &Position {
                y: ny - 1,
                x: nx - 1
            },
            &Direction::Down
        ),
        Position { y: 0, x: nx - 1 }
    );
    assert_eq!(
        world.move_to(
            &Position {
                y: ny - 1,
                x: nx - 1
            },
            &Direction::Left
        ),
        Position {
            y: ny - 1,
            x: nx - 2
        }
    );
    assert_eq!(
        world.move_to(
            &Position {
                y: ny - 1,
                x: nx - 1
            },
            &Direction::Right
        ),
        Position { y: ny - 1, x: 0 }
    );
}
