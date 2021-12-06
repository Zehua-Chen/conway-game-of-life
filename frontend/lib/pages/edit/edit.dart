import 'dart:convert';
import 'dart:html' as html;
import 'package:flutter/material.dart';
import 'configuration.dart';
import 'package:conway/widgets/widgets.dart';
import 'package:conway/schema/schema.dart' as schema;

class Edit extends StatefulWidget {
  const Edit({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _EditState();
}

class _EditState extends State<Edit> {
  ConwayFrame? _frame;

  void _onTap(Offset offset) {
    setState(() {
      if (_frame == null) {
        return;
      }

      bool current = _frame!.get(offset.dx.toInt(), offset.dy.toInt());
      _frame!.set(offset.dx.toInt(), offset.dy.toInt(), !current);
    });
  }

  void _save() {
    if (_frame == null) {
      return;
    }

    final story = schema.Story([_frame!.toPage()]);
    final json = jsonEncode(story.toJson());

    final bytes = utf8.encode(json);
    final blob = html.Blob([bytes]);
    final url = html.Url.createObjectUrlFromBlob(blob);

    final anchor = html.document.createElement('a') as html.AnchorElement
      ..href = url
      ..style.display = 'none'
      ..download = 'input.json';

    html.document.body?.children.add(anchor);

    anchor.click();

    html.document.body?.children.remove(anchor);
    html.Url.revokeObjectUrl(url);
  }

  @override
  Widget build(BuildContext context) {
    if (_frame == null) {
      return Configuration(onCreate: (size) {
        setState(() {
          _frame = ConwayFrame.fromWH(
              width: size.width.toInt(), height: size.height.toInt());
        });
      });
    }

    return Column(
      children: [
        Expanded(child: ConwayGrid(frame: _frame!, onTap: _onTap)),
        Padding(
            padding: const EdgeInsets.all(16),
            child: ElevatedButton.icon(
                label: const Text('SAVE'),
                icon: const Icon(Icons.save),
                onPressed: _save))
      ],
    );
  }
}
