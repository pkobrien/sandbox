# -*- coding: utf-8 -*-
"""
@author: Patrick K. O'Brien

This code uses my Conway's Game of Life python module, which is
available at https://github.com/pkobrien/game-of-life
"""

import gameoflife as gol
import itertools


class Farm(object):
    """Testing ground where seeds are grown and measured."""
    
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.area = width * height
        self.combinations = []
        self.grid = gol.Grid(width, height)
        self.grid_history = []
        self.land = list((x, y) for x in range(width) for y in range(height))
        self.seeds = []
        self.seed_history = []

    def plant(self, max_cells):
        """Plant seeds up to a maximum complexity of max_cells."""
        grid = self.grid
        seed_number = 0
        for n in range(max_cells):
            cell_quantity = n + 1
            for cells in itertools.combinations(self.land, cell_quantity):
                cells = gol.normalize(cells)
                if cells in self.combinations:
                    # Skip cell combinations whose normalized version has
                    # already been planted and evaluated.
                    continue
                self.combinations.append(cells)
                seed = Seed(seed_number, cells)
                seed_number += 1
                self.seeds.append(seed)
                grid.populate(cells)
                while True:
                    if grid.living in self.grid_history:
                        # The cells in grid.living have been seen before,
                        # either by the current seed or a previous one.
                        # The first seed that produces a cell pattern is
                        # considered the canonical seed and all others are
                        # variants (and most likely less interesting).
                        seed_index = self.grid_history.index(grid.living)
                        other_seed = self.seed_history[seed_index]
                        if seed is not other_seed:
                            seed.variant_of = other_seed
                            other_seed.variations.append(seed)
                        break
                    self.grid_history.append(grid.living)
                    self.seed_history.append(seed)
                    grid.cycle()
                    seed.max_living = max(seed.max_living, len(grid.living))
                seed.generations = grid.generations

    def harvest(self):
        return self.seeds


class Seed(object):
    """A GMO for engineering interesting population sets to seed a grid."""
    
    def __init__(self, number, cells):
        self.number = number
        self.cells = cells
        self.generations = 0
        self.max_living = 0
        self.variant_of = None
        self.variations = []

    @property
    def cell_count(self):
        return len(self.cells)


def sample_harvest(magic_number):
    """Return the results of farming based on the magic_number."""
    magic_number = min(magic_number, 9)
    farm = Farm(width=magic_number, height=magic_number)
    farm.plant(max_cells=magic_number)
    seeds = farm.harvest()
    return seeds


# This was the initial idea and inspiration for this file of code.
# Unfortunately, planting 5 cells takes forever, so I never got to 9,
# nor did I do any subsequent analysis of the resulting seeds. Oh well.
def sample_harvest_9():
    """Return the results of farming a 9x9 grid with up to 9 cells."""
    farm = Farm(width=9, height=9)
    farm.plant(max_cells=9)
    seeds = farm.harvest()
    return seeds
