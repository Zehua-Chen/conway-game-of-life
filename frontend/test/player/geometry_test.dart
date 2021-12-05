import 'package:test/test.dart';
import 'package:conway/schema/schema.dart';
import 'package:conway/widgets/player/geometry.dart';

void main() {
  test('bounding box', () {
    final page = Page([
      LivingCell(2, 2),
      LivingCell(-2, -2),
    ]);

    final rect = page.boundingBox;

    expect(rect.minX, -2);
    expect(rect.maxX, 2);
    expect(rect.minY, -2);
    expect(rect.maxY, 2);
  });
}
