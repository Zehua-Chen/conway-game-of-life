import 'package:test/test.dart';
import 'package:frontend/schema/schema.dart';
import 'dart:convert';

void main() {
  group('Living Cell', () {
    const json = '{"x":1,"y":2}';

    test('toJson', () {
      final actualJson = jsonEncode(LivingCell(1, 2).toJson());
      expect(actualJson, '{"x":1,"y":2}');
    });

    test('fromJson', () {
      final cell = LivingCell.fromJson(jsonDecode(json));

      expect(cell.x, 1);
      expect(cell.y, 2);
    });
  });

  group('Page', () {
    const json = '{"livingCells":[{"x":0,"y":0},{"x":1,"y":1}]}';

    test('toJson', () {
      final page = Page([LivingCell(0, 0), LivingCell(1, 2)]);
      final actualJson = jsonEncode(page.toJson());
      expect(actualJson, actualJson);
    });

    test('fromJson', () {
      final page = Page.fromJson(jsonDecode(json));

      for (int i = 0; i < 2; i++) {
        expect(page.cells[i].x, i);
        expect(page.cells[i].y, i);
      }
    });
  });

  group('Story', () {
    const json = '{"pages":[{"livingCells":[{"x":0,"y":0},{"x":1,"y":1}]}]}';

    test('toJson', () {
      final story = Story([
        Page([LivingCell(0, 0), LivingCell(1, 2)])
      ]);
      final actualJson = jsonEncode(story.toJson());
      expect(actualJson, actualJson);
    });

    test('fromJson', () {
      final story = Story.fromJson(jsonDecode(json));

      for (int i = 0; i < 2; i++) {
        expect(story.pages[0].cells[i].x, i);
        expect(story.pages[0].cells[i].y, i);
      }
    });
  });
}
