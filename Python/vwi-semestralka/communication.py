from PySide2.QtCore import QObject, Signal, Slot

signals = {
    "submit": "Run processing",
}


class Communicate(QObject):
    submit = Signal(list, str, str)     # [X_name, Y_name], order1, order2
    categories = Signal(list)
    model = Signal(object)


communication = Communicate()
