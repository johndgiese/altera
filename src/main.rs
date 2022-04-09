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

use std::cmp;
use std::collections::HashMap;
use std::fmt;

use rand::Rng;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;

type Weight = u8;

type Age = u128;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Direction {
    Up,
    Left,
    Down,
    Right,
}

use Direction::{Down, Left, Right, Up};

// TODO: use Trait Objects to allow Nooks with different implementations all to run at the same time
#[derive(Debug, Clone, Copy, PartialEq)]
struct Nook {
    weight: Weight,
    // TODO: add the internal state of the Nook which is passed between time steps
}

type View = [[Weight; 5]; 5];

/// A [Position] uniquely denotes a square on the world grid. The origin, (0,
/// 0), begins at the top-left of the array. The first coordinate indicates the
/// row (sometimes referred to as the y-axis) and the second the column (the
/// x-axis).
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Position(isize, isize);

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl Nook {
    fn act(&self, view: View) -> Action {
        // TODO: implement this
        Action::Rest
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Thing {
    Nook(Nook, Age),
    Food,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Action {
    Move(Direction),
    Eat(Direction),
    Split(Direction),
    Rest,
}

/// A World stores the complete state of a particular simulation instance. It
/// includes [Nook::map] which track of which squares have food or nooks on
/// them. It also stores the relative age of each nook that's been added to the
/// world. The order that the nooks were added in is important since the nook's
/// actions must be applied in that order.
struct World {
    // TODO: add the random number generator's state
    t: isize,
    starvation_interval: isize,
    ny: isize,
    nx: isize,
    nook_counter: Age,
    food_to_distribute: isize,
    map: HashMap<Position, Thing>,
    rng: ChaChaRng,
}

impl World {
    fn step(&mut self) {
        self.t += 1;
        // Collect all Nook's actions given the current state of the world;
        // then process them. Nook actions are all calculated upfront in order
        // to allow the process to be parallelizable.
        let mut actions: HashMap<Age, Action> = HashMap::new();
        let age = 0;
        while let Some((nook, position, age)) = self.next_nook(age) {
            let view = self.get_view(&position);
            actions.insert(age, nook.act(view));
        }
        self.apply_actions(actions);
        if self.t % self.starvation_interval == 0 {
            self.starve_nooks();
        }
        self.distribute_food();
    }

    fn next_nook(&self, younger_than: Age) -> Option<(Nook, Position, Age)> {
        // TODO: come up with a more efficient way to iterate through all of the
        // nooks in order of age (e.g., consider implementing a binary tree representation)
        self.map
            .iter()
            .filter_map(|(position, thing)| match thing {
                Thing::Food => None,
                Thing::Nook(_, age) if *age >= younger_than => None,
                Thing::Nook(nook, age) => Some((*nook, *position, *age)),
            })
            .min_by(|(_, _, lhs), (_, _, rhs)| lhs.cmp(rhs))
    }

    fn add_nook(&mut self, position: &Position, nook: Nook) {
        match self.map.get(position) {
            None => {
                self.map
                    .insert(position.clone(), Thing::Nook(nook, self.nook_counter));
                self.nook_counter += 1;
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
    fn remove_nook(&mut self, position: &Position) -> (Nook, Age) {
        match self.map.remove(position) {
            Some(Thing::Nook(nook, age)) => (nook, age),
            Some(Thing::Food) => panic!("Expected nook at {}, found food", position),
            None => panic!("Expected nook at {}, found nothing", position),
        }
    }

    fn nook_move(&mut self, position: &Position, direction: &Direction) -> bool {
        let new_position = self.move_to(&position, &direction);
        // TODO: handle pushing objects
        match self.map.get(&new_position) {
            None => {
                let thing = self.map.remove(&position).unwrap(); // move shouldn't ever be called if a nook isn't at the squre
                assert!(match thing {
                    Thing::Nook(_, _) => true,
                    _ => false,
                });
                self.map.insert(new_position, thing);
                true
            }
            Some(Thing::Food) => false,
            Some(Thing::Nook(_, _)) => false,
        }
    }

    fn nook_eat(&mut self, position: &Position, direction: &Direction) -> bool {
        let meal_position = self.move_to(&position, &direction);
        let nook = match self.map.get(&position) {
            Some(Thing::Nook(nook, _)) => nook,
            _ => panic!("No nook at position!"),
        };
        match self.map.get(&meal_position) {
            None => false,
            Some(Thing::Food) => {
                self.map.remove(&meal_position);
                self.fatten_nook(position, 1);
                true
            }
            Some(Thing::Nook(other, _)) if other.weight <= nook.weight => {
                self.fatten_nook(position, other.weight);
                self.remove_nook(&meal_position);
                true
            }
            Some(Thing::Nook(_, _)) => false,
        }
    }

    fn fatten_nook(&mut self, position: &Position, gain: Weight) {
        // TODO: figure out a way to avoid casting in this method

        // TODO: figure out how to implement this without copying the Nook
        // (e.g., using Rc<RefCel<Nook>>)
        let (mut nook, age) = match self.map.get(position) {
            Some(Thing::Nook(nook, age)) => (nook.clone(), age),
            None => panic!("Nothing at square {}", position),
            Some(Thing::Food) => panic!("Food at square {}", position),
        };
        let nook_weight: isize = nook.weight.into();
        let gain_weight: isize = gain.into();
        if nook_weight + gain_weight > 255 {
            self.food_to_distribute += nook_weight + gain_weight - 255;
            nook.weight = 255;
        } else {
            nook.weight += gain;
        }
        self.map.insert(*position, Thing::Nook(nook, *age));
    }

    fn starve_nooks(&mut self) {
        let mut positions_to_remove: Vec<Position> = vec![];
        for (position, thing) in self.map.iter_mut() {
            match thing {
                Thing::Nook(nook, _) => {
                    if nook.weight == 1 {
                        positions_to_remove.push(position.clone());
                    } else {
                        nook.weight -= 1;
                    }
                    self.food_to_distribute += 1;
                }
                _ => (),
            }
        }
        for position in positions_to_remove {
            self.map.remove(&position);
        }
    }

    fn distribute_food(&mut self) {
        assert!(self.food_to_distribute <= self.num_empty());
        while self.food_to_distribute > 0 {
            let iy = self.rng.gen_range(0..self.nx);
            let ix = self.rng.gen_range(0..self.ny);
            let position = Position(iy, ix);
            match self.map.get(&position) {
                None => {
                    self.map.insert(position, Thing::Food);
                    self.food_to_distribute -= 1;
                }
                _ => (),
            }
        }
    }

    fn get_view(&self, center: &Position) -> View {
        let mut view: View = [[0u8; 5]; 5];
        let yc = center.0;
        let xc = center.1;
        for (iy, y) in (-2..=2).enumerate() {
            for (ix, x) in (-2..=2).enumerate() {
                let yp = (yc + y).modulo(self.ny);
                let xp = (xc + x).modulo(self.nx);
                let center_modulo = Position(yp, xp);
                view[iy][ix] = self.get_weight(&center_modulo);
            }
        }
        view
    }

    fn get_weight(&self, p: &Position) -> u8 {
        match self.map.get(p) {
            None => 0,
            Some(Thing::Nook(nook, _)) => nook.weight,
            Some(Thing::Food) => 1,
        }
    }

    fn total_weight(&self) -> u128 {
        self.map.values().fold(0, |sum, thing| {
            sum + match thing {
                Thing::Nook(nook, _) => nook.weight.into(),
                Thing::Food => 1,
            }
        })
    }

    fn num_nooks(&self) -> isize {
        self.map.values().fold(0, |sum, thing| {
            sum + match thing {
                Thing::Nook(_, _) => 1,
                Thing::Food => 0,
            }
        })
    }

    fn num_empty(&self) -> isize {
        self.nx * self.ny
            - self.map.values().fold(0, |sum, thing| {
                sum + match thing {
                    Thing::Nook(_, _) => 1,
                    Thing::Food => 1,
                }
            })
    }

    fn move_to(&self, start: &Position, direction: &Direction) -> Position {
        match direction {
            Down => Position((start.0 + 1).modulo(self.ny), start.1),
            Up => Position((start.0 - 1).modulo(self.ny), start.1),
            Left => Position(start.0, (start.1 - 1).modulo(self.nx)),
            Right => Position(start.0, (start.1 + 1).modulo(self.nx)),
        }
    }

    fn apply_actions(&mut self, actions: HashMap<Age, Action>) {
        let age = 0;
        while let Some((nook, position, age)) = self.next_nook(age) {
            match actions.get(&age) {
                None => (), // newly added nook
                Some(Action::Rest) => (),
                Some(Action::Move(direction)) => {
                    self.nook_move(&position, &direction);
                }
                Some(Action::Split(direction)) => {
                    // TODO: handle
                    //   see what's on the square
                    //   if there's nothing there, create a new Nook with 1/2 the weight rounding down
                }
                Some(Action::Eat(direction)) => {
                    self.nook_eat(&position, &direction);
                }
            }
        }
    }

    fn print(&self) {
        for y in 0..self.ny {
            for x in 0..self.nx {
                let position = Position(y, x);
                let thing = self.map.get(&position);
                match thing {
                    None => print!(" "),
                    Some(Thing::Food) => print!("."),
                    Some(Thing::Nook(nook, _)) => print!("{}", cmp::min(nook.weight, 9)),
                }
            }
            println!("");
        }
    }
}

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

fn single_nook_world(ny: isize, nx: isize) -> World {
    let mut world = blank_world(ny, nx);
    for y in 0..ny {
        for x in 0..nx {
            let position = Position(y, x);
            if x == (nx - 1) / 2 && y == (ny - 1) / 2 {
                let nook = Nook { weight: 1 };
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
    F: Fn(Position) -> Nook,
{
    let mut world = blank_world(ny, nx);
    for y in 0..ny {
        for x in 0..nx {
            let position = Position(y, x);
            world.add_nook(&position, func(position));
        }
    }
    world
}

fn blank_world(ny: isize, nx: isize) -> World {
    let mut map = HashMap::new();
    let t = 0;
    let nook_counter = 1;
    let food_to_distribute = 0;
    let starvation_interval = 100;
    let seed: [u8; 32] = [0; 32];
    let rng = ChaChaRng::from_seed(seed);
    let mut world = World {
        t,
        ny,
        nx,
        starvation_interval,
        nook_counter,
        food_to_distribute,
        map,
        rng,
    };
    world
}

#[test]
fn test_single_nook_world() {
    let world = single_nook_world(5, 5);
    assert_eq!(world.total_weight(), 25);
    assert_eq!(world.num_nooks(), 1);
    let nook = Nook { weight: 1 };
    let position = Position(2, 2);
    let age = 1;
    assert_eq!(world.map.get(&position), Some(&Thing::Nook(nook, age)));
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
    let world = world_from_func(10, 10, |p| Nook {
        weight: p.1.try_into().unwrap_or(0),
    });
    assert_eq!(
        world.get_view(&Position(2, 2)),
        [
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
            [0, 1, 2, 3, 4],
        ]
    );
    assert_eq!(
        world.get_view(&Position(2, 0)),
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
    let nook = Nook { weight: 1 };
    assert_eq!(world.num_nooks(), 0);
    world.add_nook(&Position(0, 0), nook);
    assert_eq!(world.num_nooks(), 1);
    world.remove_nook(&Position(0, 0));
    assert_eq!(world.num_nooks(), 0);
}

#[test]
fn test_move_to() {
    let ny = 4;
    let nx = 3;
    let world = blank_world(ny, nx);
    assert_eq!(world.move_to(&Position(0, 0), &Up), Position(ny - 1, 0));
    assert_eq!(world.move_to(&Position(0, 0), &Down), Position(1, 0));
    assert_eq!(world.move_to(&Position(0, 0), &Left), Position(0, nx - 1));
    assert_eq!(world.move_to(&Position(0, 0), &Right), Position(0, 1));
    assert_eq!(
        world.move_to(&Position(ny - 1, nx - 1), &Up),
        Position(ny - 2, nx - 1)
    );
    assert_eq!(
        world.move_to(&Position(ny - 1, nx - 1), &Down),
        Position(0, nx - 1)
    );
    assert_eq!(
        world.move_to(&Position(ny - 1, nx - 1), &Left),
        Position(ny - 1, nx - 2)
    );
    assert_eq!(
        world.move_to(&Position(ny - 1, nx - 1), &Right),
        Position(ny - 1, 0)
    );
}

#[test]
fn test_nook_move() {
    let ny = 4;
    let nx = 3;
    let mut world = blank_world(ny, nx);
    let nook = Nook { weight: 1 };
    world.add_nook(&Position(0, 0), nook);
    world.nook_move(&Position(0, 0), &Down);
    assert_eq!(world.map.get(&Position(0, 0)), None);
    assert_eq!(world.map.get(&Position(1, 0)), Some(&Thing::Nook(nook, 1)));
}

#[test]
fn test_nook_eat_food() {
    let mut world = blank_world(3, 3);
    world.add_nook(&Position(1, 1), Nook { weight: 1 });
    world.add_food(&Position(1, 2));
    world.nook_eat(&Position(1, 1), &Right);
    assert_eq!(world.get_weight(&Position(1, 1)), 2);
}

#[test]
fn test_nook_eat_nook() {
    let mut world = blank_world(3, 3);
    world.add_nook(&Position(1, 1), Nook { weight: 1 });
    world.add_nook(&Position(1, 2), Nook { weight: 1 });
    world.nook_eat(&Position(1, 1), &Right);
    assert_eq!(world.get_weight(&Position(1, 1)), 2);
    assert_eq!(world.get_weight(&Position(1, 2)), 0);
}

#[test]
fn test_nook_eat_heavy_nook() {
    let mut world = blank_world(3, 3);
    world.add_nook(&Position(1, 1), Nook { weight: 1 });
    world.add_nook(&Position(1, 2), Nook { weight: 2 });
    world.nook_eat(&Position(1, 1), &Right);
    assert_eq!(world.get_weight(&Position(1, 1)), 1);
    assert_eq!(world.get_weight(&Position(1, 2)), 2);
}

#[test]
fn test_nook_eat_empty() {
    let mut world = blank_world(3, 3);
    world.add_nook(&Position(1, 1), Nook { weight: 1 });
    world.nook_eat(&Position(1, 1), &Right);
    assert_eq!(world.get_weight(&Position(1, 1)), 1);
}

#[test]
fn test_nook_eat_overflow_food() {
    let mut world = blank_world(3, 3);
    world.add_nook(&Position(1, 1), Nook { weight: 255 });
    world.add_food(&Position(1, 2));
    world.nook_eat(&Position(1, 1), &Right);
    assert_eq!(world.get_weight(&Position(1, 1)), 255);
    assert_eq!(world.food_to_distribute, 1);
}

#[test]
fn test_distribute_food_blank() {
    let mut world = blank_world(5, 5);
    world.food_to_distribute = 25;
    world.distribute_food();
    assert_eq!(world.food_to_distribute, 0);
    assert_eq!(world.total_weight(), 25);
}

#[test]
fn test_starve_nooks() {
    let mut world = blank_world(5, 5);
    world.add_nook(&Position(0, 0), Nook { weight: 1 });
    world.add_nook(&Position(0, 1), Nook { weight: 2 });
    world.starve_nooks();
    assert_eq!(world.food_to_distribute, 2);
    assert_eq!(world.get_weight(&Position(0, 0)), 0);
    assert_eq!(world.get_weight(&Position(0, 1)), 1);
}

#[test]
fn test_starving_single_nook_after_interval() {
    let mut world = blank_world(5, 5);
    world.starvation_interval = 5;
    world.add_nook(&Position(0, 0), Nook { weight: 1 });
    world.step();
    world.step();
    world.step();
    world.step();
    assert_eq!(world.num_nooks(), 1);
    assert_eq!(world.total_weight(), 1);
    assert_eq!(world.t, 4);
    world.step();
    assert_eq!(world.num_nooks(), 0);
    assert_eq!(world.total_weight(), 1);
}
