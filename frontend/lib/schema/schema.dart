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

  Map<String, dynamic> toJson() {
    return {'livingCells': cells.map((cell) => cell.toJson()).toList()};
  }
}

class Story {
  List<Page> pages = [];

  Story(this.pages);

  Story.fromJson(Map<String, dynamic> json) {
    for (final page in json['pages']) {
      pages.add(Page.fromJson(page));
    }
  }

  Map<String, dynamic> toJson() {
    return {'pages': pages.map((page) => page.toJson()).toList()};
  }
}
