import 'package:test/test.dart';
import 'package:conway/schema/schema.dart';

void main() {
  test('page to frame', () {
    final frame = Page([
      LivingCell(1, 1),
      // LivingCell(0, 0),
    ]).toFrame();

    expect(frame.width, 3);
    expect(frame.height, 3);

    expect(frame.minX, -1);
    expect(frame.maxX, 1);

    expect(frame.minY, -1);
    expect(frame.maxY, 1);

    expect(frame.get(1, 1), true);
    expect(frame.get(0, 0), false);
    expect(frame.get(-1, -1), false);
  });
}
