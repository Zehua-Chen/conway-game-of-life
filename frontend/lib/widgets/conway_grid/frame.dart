class ConwayFrame {
  int width = 0;
  int height = 0;

  List<bool> _cells = [];

  ConwayFrame.fromNestedList(List<List<bool>> rows) {
    height = rows.length;
    assert(height % 2 != 0);

    width = rows[0].length;
    assert(width % 2 != 0);

    _cells = [];

    for (List<bool> row in rows) {
      _cells.addAll(row);
    }
  }

  ConwayFrame.fromWH(
      {required this.width, required this.height, bool alive = false})
      : assert(width % 2 != 0),
        assert(height % 2 != 0) {
    _cells = [];

    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        _cells.add(alive);
      }
    }
  }

  List<bool> get cells => _cells;

  int get _centerXOffset {
    return (width / 2).floor();
  }

  int get _centerYOffset {
    return (height / 2).floor();
  }

  int get minX {
    return -_centerXOffset;
  }

  int get minY {
    return -_centerYOffset;
  }

  int get maxX {
    return _centerXOffset;
  }

  int get maxY {
    return _centerYOffset;
  }

  bool get(int x, int y) {
    x += _centerXOffset;
    y += _centerYOffset;

    return _cells[y * width + x];
  }

  void set(int x, int y, bool alive) {
    x += _centerXOffset;
    y += _centerYOffset;

    _cells[y * width + x] = alive;
  }
}
