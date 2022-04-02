mod modulo;
use modulo::Mod;
mod linked_list;
use linked_list::LinkedList;

use std::cmp;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

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
    map: HashMap<Position, Thing>,
    nooks: LinkedList<Rc<Nook>>,
}

impl World {
    fn step(&mut self) {
        let num_actions = self.num_nooks().try_into().unwrap();
        let mut actions: Vec<Action> = Vec::with_capacity(num_actions);
        for (position, thing) in self.map.iter() {
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

    fn add_nook(&mut self, position: &Position, nook: Rc<Nook>) {
        match self.map.get(position) {
            None => {
                self.nooks.push_back(Rc::clone(&nook));
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
            Some(other) if Rc::ptr_eq(&nook, other) => false,
            _ => true,
        } {}
        cursor.prev().unwrap();
        cursor.remove().unwrap();
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
    // data structure that
    // - lets you see who is in a particular location (either HashMap or a big array of pointers)
    // - lets you iterate through nooks in order (linked list)
    // - add new nooks at locations (ix, iy) -> Nook
    // - remove nooks during an iteration
    // - move nooks to new locations while preserving the iteration order

    fn apply_actions(&self, actions: Vec<Action>) {
        // for each nook (in order of creation)
        //  if rest, continue
        //  if eat
        //   see what's on the square
        //   if it's food, increase weight
        //   if it's a nook and it weights less, eat the nook
        //  if move
        //   see what's on the square
        //   if there's something there, skip turn (TODO: implement pushing)
        //   if there's nothing there, move the nook
        //  if split
        //   see what's on the square
        //   if there's nothing there, create a new Nook with 1/2 the weight rounding down
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

fn single_nook_world(nx: isize, ny: isize) -> World {
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
    let position = Position { x: 2, y: 2 };
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
