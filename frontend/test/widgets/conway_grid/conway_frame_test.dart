import 'package:test/test.dart';
import 'package:conway/widgets/conway_grid/conway_grid.dart';

void main() {
  group('cell representation', () {
    test('get', () {
      final frame = ConwayFrame.fromWH(width: 3, height: 3, alive: false);
      frame.set(1, 1, true);
      frame.set(0, 1, true);

      expect(frame.get(1, 1), true);
      expect(frame.get(0, 1), true);

      expect(frame.get(0, 0), false);
    });
  });
}
