class LivingCell {
  int x = 0;
  int y = 0;

  LivingCell(this.x, this.y);

  LivingCell.fromJson(Map<String, dynamic> json) {
    x = json['x'];
    y = json['y'];
  }

  Map<String, dynamic> toJson() {
    return {'x': x, 'y': y};
  }
}

class Page {
  List<LivingCell> cells = [];

  Page(this.cells);

  Page.fromJson(Map<String, dynamic> json) {
    for (final cell in json['livingCells']) {
      cells.add(LivingCell.fromJson(cell));
    }
  }

  int get length {
    return cells.length;
  }

  Map<String, dynamic> toJson() {
    return {'livingCells': cells.map((cell) => cell.toJson()).toList()};
  }

  bool isAlive(int x, int y) {
    for (final cell in cells) {
      if (cell.x == x && cell.y == y) {
        return true;
      }
    }

    return false;
  }
}

class Story {
  List<Page> pages = [];

  Story(this.pages);

  int get length {
    return pages.length;
  }

  Story.fromJson(Map<String, dynamic> json) {
    for (final page in json['pages']) {
      pages.add(Page.fromJson(page));
    }
  }

  Map<String, dynamic> toJson() {
    return {'pages': pages.map((page) => page.toJson()).toList()};
  }
}
