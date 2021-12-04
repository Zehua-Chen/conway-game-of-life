import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';
import 'row.dart';

class ConwayPlayer extends StatefulWidget {
  const ConwayPlayer({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ConwayPlayerState();
}

class _ConwayPlayerState extends State<ConwayPlayer> {
  bool _playing = false;

  _toggle() {
    setState(() {
      _playing = !_playing;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Column(children: [
      Expanded(
          child: Column(
              children: [
            for (int i = 0; i < 10; i++) const ConwayRow(count: 10),
          ],
              mainAxisAlignment: MainAxisAlignment.center,
              crossAxisAlignment: CrossAxisAlignment.center)),
      Padding(
          padding: const EdgeInsets.all(16.0),
          child: IconButton(
              icon: Icon(_playing ? Icons.pause : Icons.play_arrow),
              onPressed: _toggle))
    ]);
  }
}
