#![allow(dead_code)]

use std::ops::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vector {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Vector {
    pub const ZERO: Self = Self::splat(0.0);
    pub const X: Self = Self::new(1.0, 0.0, 0.0);
    pub const Y: Self = Self::new(0.0, 1.0, 0.0);
    pub const Z: Self = Self::new(0.0, 0.0, 1.0);
    #[inline]
    pub const fn new(x: f64, y: f64, z: f64) -> Self {
        Self { x, y, z }
    }
    #[inline]
    pub const fn splat(v: f64) -> Self {
        Self { x: v, y: v, z: v }
    }
    pub fn length(self) -> f64 {
        self.length_squared().sqrt()
    }
    pub fn length_squared(self) -> f64 {
        self.dot(self)
    }
    pub fn dot(self, other: Self) -> f64 {
        self.x
            .mul_add(other.x, self.y.mul_add(other.y, self.z * other.z))
    }
    pub fn unit(self) -> Self {
        if self.length_squared() == 0.0 {
            Self::ZERO
        } else {
            self / self.length()
        }
    }
    pub fn cross(self, other: Self) -> Self {
        Self::new(
            self.y * other.z - self.z * other.y,
            self.z * other.x - self.x * other.z,
            self.x * other.y - self.y * other.x,
        )
    }
}

macro_rules! bin_op {
    ($trait:ident, $method:ident, $assign_trait:ident, $assign_method:ident) => {
        impl $trait for Vector {
            type Output = Self;
            fn $method(mut self, other: Self) -> Self {
                self.$assign_method(other);
                self
            }
        }

        impl $assign_trait for Vector {
            fn $assign_method(&mut self, other: Self) {
                self.x.$assign_method(other.x);
                self.y.$assign_method(other.y);
                self.z.$assign_method(other.z);
            }
        }

        impl $trait<f64> for Vector {
            type Output = Self;
            fn $method(mut self, other: f64) -> Self {
                self.$assign_method(Self::splat(other));
                self
            }
        }

        impl $assign_trait<f64> for Vector {
            fn $assign_method(&mut self, other: f64) {
                self.$assign_method(Self::splat(other));
            }
        }
    };
}

bin_op!(Add, add, AddAssign, add_assign);
bin_op!(Sub, sub, SubAssign, sub_assign);
bin_op!(Mul, mul, MulAssign, mul_assign);
bin_op!(Div, div, DivAssign, div_assign);

impl Neg for Vector {
    type Output = Self;
    fn neg(self) -> Self {
        Self::new(-self.x, -self.y, -self.z)
    }
}

pub struct RectPrism {
    pub tlb: Vector,
    pub size: Vector,
}

impl RectPrism {
    pub fn from_min_max(min: Vector, max: Vector) -> Self {
        Self {
            tlb: min,
            size: max - min,
        }
    }
    pub fn contains(&self, p: Vector) -> bool {
        let min = self.tlb;
        let max = self.tlb + self.size;
        (min.x..=max.x).contains(&p.x)
            && (min.y..=max.y).contains(&p.y)
            && (min.z..=max.z).contains(&p.z)
    }
}
