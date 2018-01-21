type t('a) = {
    mutable data: array('a),
    mutable bufferSize: int,
    mutable size: int
};
let make = (default) => {
    let bufferSize = 30;
    {
        data: Array.make(bufferSize, default),
        bufferSize,
        size: 0
    }
};

let push = (self, element) => {
    if (self.size >= self.bufferSize) {
        /* Double buffer size */
        self.data = Array.append(self.data, Array.make(self.bufferSize, 0));
        self.bufferSize = self.bufferSize + self.bufferSize;
    };
    self.data[self.size] = element;
    self.size = self.size + 1;
};
let push2 = (self, el1, el2) => {
    if (self.size + 2 > self.bufferSize) {
        /* Double buffer size */
        self.data = Array.append(self.data, Array.make(self.bufferSize, 0));
        self.bufferSize = self.bufferSize + self.bufferSize;
    };
    self.data[self.size] = el1;
    self.data[self.size + 1] = el2;
    self.size = self.size + 2;
};
let push3 = (self, el1, el2, el3) => {
    if (self.size + 3 > self.bufferSize) {
        /* Double buffer size */
        self.data = Array.append(self.data, Array.make(self.bufferSize, 0));
        self.bufferSize = self.bufferSize + self.bufferSize;
    };
    self.data[self.size] = el1;
    self.data[self.size + 1] = el2;
    self.data[self.size + 2] = el3;
    self.size = self.size + 3;
};
let push4 = (self, el1, el2, el3, el4) => {
    if (self.size + 4 > self.bufferSize) {
        /* Double buffer size */
        self.data = Array.append(self.data, Array.make(self.bufferSize, 0));
        self.bufferSize = self.bufferSize + self.bufferSize;
    };
    self.data[self.size] = el1;
    self.data[self.size + 1] = el2;
    self.data[self.size + 2] = el3;
    self.data[self.size + 3] = el4;
    self.size = self.size + 4;
};

let toArray = (self) => {
    Array.sub(self.data, 0, self.size)
};