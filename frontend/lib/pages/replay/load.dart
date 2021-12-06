import 'dart:convert';
import 'package:flutter/widgets.dart';
import 'package:flutter_dropzone/flutter_dropzone.dart';
import 'package:conway/schema/schema.dart' as schema;
import 'package:conway/widgets/conway_grid/conway_grid.dart';

class Load extends StatefulWidget {
  ValueSetter<List<ConwayFrame>> onFramesOpened;

  Load({Key? key, required this.onFramesOpened}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _LoadState();
}

class _LoadState extends State<Load> {
  DropzoneViewController? _controller;

  void _onDrop(dynamic file) async {
    if (_controller == null) {
      return;
    }

    final data = await _controller!.getFileData(file);
    final json = utf8.decode(data);
    final story = schema.Story.fromJson(jsonDecode(json));

    widget.onFramesOpened.call(story.toFrames());
  }

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        DropzoneView(
            onCreated: (controller) => _controller = controller,
            onDrop: _onDrop),
        Column(
          mainAxisAlignment: MainAxisAlignment.center,
          crossAxisAlignment: CrossAxisAlignment.center,
          children: const <Widget>[Text('Drop File')],
        )
      ],
    );
  }
}
