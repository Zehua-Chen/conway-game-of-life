import 'package:flutter/material.dart';
import 'pages/replay/replay.dart';
import 'pages/edit/edit.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
        title: 'Conway Game of Life',
        theme: ThemeData(primarySwatch: Colors.deepPurple),
        home: const MyStatefulWidget());
  }
}

class MyStatefulWidget extends StatefulWidget {
  const MyStatefulWidget({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _MyStatefulWidgetState();
}

class _MyStatefulWidgetState extends State<MyStatefulWidget> {
  int _selectedIndex = 0;

  static Route<Object?> _dialogBuilder(
      BuildContext context, Object? arguments) {
    return DialogRoute<void>(
        context: context,
        builder: (BuildContext context) {
          return const AboutDialog(
              applicationName: 'Conway Game of Life',
              applicationVersion: '1.0.0');
        });
  }

  void _onIndexChange(int index) {
    setState(() {
      _selectedIndex = index;
    });
  }

  String _title() {
    switch (_selectedIndex) {
      case 0:
        return 'Create Conway Game of Life';
      case 1:
        return 'Replay Conway Game of Life';
      default:
        throw Error();
    }
  }

  Widget _body() {
    switch (_selectedIndex) {
      case 0:
        return const Edit();
      case 1:
        return const Replay();
      default:
        return const Placeholder();
    }
  }

  @override
  Widget build(BuildContext context) {
    final desktop = MediaQuery.of(context).size.width >= 700;

    return Scaffold(
        appBar: AppBar(
            // Here we take the value from the MyHomePage object that was created by
            // the App.build method, and use it to set our appbar title.
            title: Text(_title()),
            actions: <Widget>[
              IconButton(
                  icon: const Icon(Icons.info),
                  onPressed: () {
                    Navigator.of(context).restorablePush(_dialogBuilder);
                  })
            ]),
        body: Row(
          children: [
            if (desktop)
              NavigationRail(
                selectedIndex: _selectedIndex,
                labelType: NavigationRailLabelType.selected,
                onDestinationSelected: _onIndexChange,
                destinations: const [
                  NavigationRailDestination(
                      icon: Icon(Icons.create), label: Text('Create')),
                  NavigationRailDestination(
                      icon: Icon(Icons.replay), label: Text('Replay')),
                ],
              ),
            Expanded(child: Center(child: _body()))
          ],
        ),
        bottomNavigationBar: desktop
            ? null
            : BottomNavigationBar(items: const [
                BottomNavigationBarItem(
                    icon: Icon(Icons.create), label: 'Create'),
                BottomNavigationBarItem(
                    icon: Icon(Icons.create), label: 'Replay')
              ], currentIndex: _selectedIndex, onTap: _onIndexChange));
  }
}
