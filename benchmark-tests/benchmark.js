function cons (x, y) {this.x = x; this.y = y}

require ('sys').puts ('cons1');
for (var i = 0; i < 8000000; ++i) new cons (3, 5);

require ('sys').puts ('cons2');
var conses = [];
for (var i = 0; i < 8000000; ++i) {
  conses.push (3);
  conses.push (5);
}
