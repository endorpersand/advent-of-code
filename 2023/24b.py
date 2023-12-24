import collections.abc
import dataclasses
from fractions import Fraction
import itertools
from typing import Self

START = 7
END = 27
START = 200_000_000_000_000
END = 400_000_000_000_000

@dataclasses.dataclass
class Intersect:
    t0: Fraction # time first hailstone intersects
    t1: Fraction # time second hailstone intersects
    pos: tuple[Fraction, Fraction] # fraction hailstones intersect

@dataclasses.dataclass
class Hailstone:
    pos: tuple[int, int, int]
    vel: tuple[int, int, int]

    @classmethod
    def parse(cls, line: str) -> Self:
        pos_str, _, vel_str = line.partition(" @ ")
        
        pos = tuple(int(s.strip()) for s in pos_str.split(", "))
        vel = tuple(int(s.strip()) for s in vel_str.split(", "))

        return cls(pos=pos, vel=vel)

    def intersect2d(self, other: Self) -> Intersect | None:
        x0, y0, _ = self.pos
        vx0, vy0, _ = self.vel
        x1, y1, _ = other.pos
        vx1, vy1, _ = other.vel

        denom = vx0 * vy1 - vy0 * vx1
        if denom == 0: return None

        t = Fraction((y0 - y1) * vx1 - (x0 - x1) * vy1, denom)
        u = Fraction((y0 - y1) * vx0 - (x0 - x1) * vy0, denom)

        x = x0 + vx0 * t
        y = y0 + vy0 * t

        return Intersect(
            t0 = t,
            t1 = u,
            pos = [x, y]
        )
    
    def shift_vel2(self, delta: tuple[int, int]) -> Self:
        vx, vy, vz = self.vel
        dvx, dvy = delta

        return self.__class__(self.pos, (vx - dvx, vy - dvy, vz))

def parse(file: str) -> list[Hailstone]:
    return [Hailstone.parse(line) for line in file.splitlines()]

with open("inputs/24.txt") as f:
    stones = parse(f.read())

# PART A
intersecting = 0
for s0, s1 in itertools.combinations(stones, 2):
    intersect = s0.intersect2d(s1)
    if (intersect is not None and
        intersect.t0 >= 0 and
        intersect.t1 >= 0 and
        START <= intersect.pos[0] <= END and
        START <= intersect.pos[1] <= END
        ):
        intersecting += 1
print(intersecting)

# PART B
@dataclasses.dataclass(init=False)
class LatticeIter(collections.abc.Iterator):
    mag: int
    last: tuple[int, int]
    delta: tuple[int, int]

    def __init__(self):
        self.mag = 0
        self.last = (0, 0)
        self.delta = (0, 0)

    def __next__(self) -> (int, int):
        if self.last == (0, 0):
            pass
        elif self.last == (self.mag, 0):
            self.delta = (-1, 1)
        elif self.last == (0, self.mag):
            self.delta = (-1, -1)
        elif self.last == (-self.mag, 0):
            self.delta = (1, -1)
        elif self.last == (0, -self.mag):
            self.delta = (1, 1)
        
        x, y = self.last
        dx, dy = self.delta
        self.last = (x + dx, y + dy)
        if self.last == (self.mag, 0):
            self.last = (self.mag + 1, 0)
            self.mag += 1

        return self.last

s0, s1, s2, *_ = stones
# Iterate through possible 2D velocities for the rock
for rv2 in LatticeIter():
    # Shift the velocities of the hailstone by the potential vel of the rock.
    s0s = s0.shift_vel2(rv2)
    s1s = s1.shift_vel2(rv2)
    s2s = s2.shift_vel2(rv2)

    # Find the intersction point
    # If all 3 intersect at the same position,
    # this is a likely vel (of rock), initial pos (of rock), time of intersection.
    s01i = s0s.intersect2d(s1s)
    s02i = s0s.intersect2d(s2s)
    
    if s01i is not None and s02i is not None and s01i.pos == s02i.pos:
        # print(rv2)

        t0 = s01i.t0
        t1 = s01i.t1
        t2 = s02i.t1
        p  = s01i.pos
        # print(f"rock's 2d position: {p}")
        # print(f"intersect time for {s0}: {t0}")
        # print(f"intersect time for {s1}: {t1}")
        # print(f"intersect time for {s2}: {t2}")

        # finding z
        # zr + t0 vzr = zh0 + t0 vzh0
        # zr + t1 vzr = zh1 + t1 vzh1
        x, y = p
        z0 = s0.pos[2] + t0 * s0.vel[2]
        z1 = s1.pos[2] + t1 * s1.vel[2]

        rvz = (z1 - z0) / (t1 - t0)
        z = z0 - t0 * rvz
        # print(f"rock's z position: {z}")
        # print(f"rock's z velocity: {rvz}")

        print(f"{x + y + z}")
        break