pub struct Fuel {
    tank: i32,
    interrupted: bool,
    unlimited: bool,
}

impl Fuel {
    pub fn new(fuel: i32) -> Self {
        Fuel {
            tank: fuel,
            interrupted: false,
            unlimited: false,
        }
    }

    pub fn unlimited() -> Self {
        Fuel {
            tank: i32::MAX,
            interrupted: false,
            unlimited: true,
        }
    }

    pub fn adjust(&mut self, fuel: i32) {
        self.tank = self.tank.saturating_add(fuel);
    }

    pub fn consume(&mut self, fuel: i32) {
        self.adjust(fuel.saturating_neg());
    }

    pub fn should_continue(&self) -> bool {
        self.unlimited || self.tank > 0 && !self.interrupted
    }
}
