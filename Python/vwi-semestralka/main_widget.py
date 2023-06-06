from PySide2.QtCore import Qt
from PySide2.QtWidgets import (QWidget, QHBoxLayout, QSplitter)
from widgets.graph_widget import Graph
from widgets.table_widget import Table
from widgets.picker_widget import Picker

from table_car_model import CustomTableModel


class Widget(QWidget):
    def __init__(self, data):
        QWidget.__init__(self)

        # Getting the Model
        self.model = CustomTableModel(data)

        self.graph = Graph(self.model)
        self.table = Table(self.model)
        self.list = Picker(data)

        # Layout
        self.splitter_h = QSplitter(Qt.Horizontal)
        self.splitter_v = QSplitter(Qt.Vertical)

        self.splitter_h.addWidget(self.table)
        self.splitter_h.setStretchFactor(0, 2)

        self.splitter_h.addWidget(self.graph)
        self.splitter_h.setStretchFactor(1, 3)

        self.splitter_v.addWidget(self.splitter_h)
        self.splitter_v.addWidget(self.list)
        self.splitter_v.setStretchFactor(0, 8)
        self.splitter_v.setStretchFactor(1, 2)

        # Set the layout to the QWidget
        layout = QHBoxLayout()
        layout.addWidget(self.splitter_v)
        self.setLayout(layout)


