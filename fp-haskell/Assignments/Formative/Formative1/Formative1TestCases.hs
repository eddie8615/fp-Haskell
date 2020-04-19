module Formative1TestCases (TestCase (TestCase, grid, name, link), cases) where

data TestCase = TestCase { grid :: [(Int, Int)], name :: String, link :: String }

cases :: [TestCase]
cases = [
  block2,
  beehive,
  tub,
  blinker,
  toad,
  beacon,
  pulsar,
  figure_eight,
  pentadecathlon,
  glider,
  lightweight_spaceship,
  block3,
  pentagenarian,
  s_pentomino,
  u_pentomino,
  y_pentomino,
  z_pentomino,
  empty,
  dot,
  domino,
  block3,
  die_hard,
  pentagenarian
  ]

-- Still lives
block2 =
  TestCase {
    grid = [(x, y) | x <- [1..2], y <- [1..2]],
    name = "block2",
    link = "http://conwaylife.com/wiki/Block"
  }

beehive =
  TestCase {
    grid = [(1, 2), (1, 3), (2, 1), (2, 4), (3, 2), (3, 3)],
    name = "beehive",
    link = "http://conwaylife.com/wiki/Beehive"
  }

tub =
  TestCase {
    grid = [(1, 2), (2, 1), (2, 3), (3, 2)],
    name = "tub",
    link = "http://conwaylife.com/wiki/Tub"
  }

-- Oscillators
blinker =
  TestCase {
    grid = [(1, 1), (1, 2), (1, 3)],
    name = "blinker",
    link = "http://conwaylife.com/wiki/Blinker"
  }

toad =
  TestCase {
    grid = [(1, 2), (1, 3), (1, 4), (2, 1), (2, 2), (2, 3)],
    name = "toad",
    link = "http://conwaylife.com/wiki/Toad"
  }

beacon =
  TestCase {
    grid = [(x, y) | x <- [1, 2], y <- [1, 2]] ++ [(x, y) | x <- [3, 4], y <- [3, 4]],
    name = "beacon",
    link = ""
  }

pulsar =
  TestCase {
    grid = [(x + i, y) | x <- [2, 8], y <- [0, 5, 7, 12], i <- [0..2]] ++ [(x, y + i) | x <- [0, 5, 7, 12], y <- [2, 8], i <- [0..2]],
    name = "pulsar",
    link = ""
  }

figure_eight =
  TestCase {
    grid = [(x, y) | x <- [1..3], y <- [1..3]] ++ [(x, y) | x <- [4..6], y <- [4..6]],
    name = "figure_eight",
    link = "http://conwaylife.com/wiki/Figure_eight"
  }

pentadecathlon =
  TestCase {
    grid = [(1, 2), (2, 2), (3, 1), (3, 3), (4, 2), (5, 2), (6, 2), (7, 2), (8, 1), (8, 3), (9, 2), (10, 2)],
    name = "pentadecathlon",
    link = "http://conwaylife.com/wiki/Pentadecathlon"
  }

-- Spaceships
glider =
  TestCase {
    grid = [(1, 3), (2, 1), (2, 3), (3, 2), (3, 3)],
    name = "glider",
    link = "http://conwaylife.com/wiki/Glider"
  }

lightweight_spaceship =
  TestCase {
    grid = [(1, 2), (1, 3), (1, 4), (2, 1), (2, 4), (3, 4), (4, 4), (5, 1), (5, 3)],
    name = "lightweight_spaceship",
    link = "http://conwaylife.com/wiki/Lightweight_spaceship"
  }

-- Pentominoes
s_pentomino =
  TestCase {
    grid = [(1, 2), (2, 2), (3, 1), (3, 2), (4, 1)],
    name = "s-pentomino",
    link = "http://www.conwaylife.com/wiki/Pentomino#Pentominoes"
  }

u_pentomino =
  TestCase {
    grid = [(1, 1), (1, 2), (2, 2), (3, 1), (3, 2)],
    name = "u-pentomino",
    link = "http://www.conwaylife.com/wiki/Pentomino#Pentominoes"
  }

y_pentomino =
  TestCase {
    grid = [(1, 2), (2, 2), (3, 1), (3, 2), (4, 2)],
    name = "y-pentomino",
    link = "http://www.conwaylife.com/wiki/Pentomino#Pentominoes"
  }

z_pentomino =
  TestCase {
    grid = [(1, 1), (2, 1), (2, 2), (2, 3), (3, 3)],
    name = "z-pentomino",
    link = "http://www.conwaylife.com/wiki/Pentomino#Pentominoes"
  }

-- Others
empty =
  TestCase {
    grid = [],
    name = "empty",
    link = ""
  }

dot =
  TestCase {
    grid = [(1, 1)],
    name = "dot",
    link = ""
}

domino =
  TestCase {
    grid = [(1, 1), (1, 2)],
    name = "domino",
    link = ""
  }

block3 =
  TestCase {
    grid = [(x, y) | x <- [1..3], y <- [1..3]],
    name = "block3",
    link = ""
  }

die_hard =
  TestCase {
    grid = [(1, 2), (2, 2), (2, 3), (6, 3), (7, 1), (7, 3), (8, 3)],
    name = "die_hard",
    link = ""
  }

pentagenarian =
  TestCase {
    grid = [(1, 2), (2, 2), (2, 3), (4, 1), (4, 3)],
    name = "pentagenarian",
    link = ""
  }
