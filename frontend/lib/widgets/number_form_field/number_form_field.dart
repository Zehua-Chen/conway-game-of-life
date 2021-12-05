import 'package:flutter/material.dart';

class NumberFormField extends StatefulWidget {
  final int value;
  final ValueChanged<int> onValueChanged;
  final Widget? label;

  const NumberFormField(
      {Key? key, required this.value, required this.onValueChanged, this.label})
      : super(key: key);

  @override
  State<StatefulWidget> createState() => _NumberFormFieldState();
}

class _NumberFormFieldState extends State<NumberFormField> {
  TextEditingController? _controller;

  @override
  void initState() {
    super.initState();

    _controller = TextEditingController(text: '${widget.value}');
    _controller?.addListener(() {
      final number = int.tryParse(_controller!.text);

      if (number != null) {
        widget.onValueChanged.call(number);
      }
    });
  }

  @override
  void dispose() {
    super.dispose();

    _controller?.dispose();
  }

  @override
  void didUpdateWidget(covariant NumberFormField oldWidget) {
    super.didUpdateWidget(oldWidget);

    if (_controller != null && oldWidget.value != widget.value) {
      _controller!.value = _controller!.value.copyWith(text: '${widget.value}');
    }
  }

  @override
  Widget build(BuildContext context) {
    return TextFormField(
        controller: _controller,
        decoration: InputDecoration(filled: true, label: widget.label));
  }
}
