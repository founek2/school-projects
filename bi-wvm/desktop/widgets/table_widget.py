from PySide2.QtCore import QDateTime, Qt
from PySide2.QtGui import QPainter
from PySide2.QtWidgets import (QWidget, QHeaderView, QHBoxLayout, QTableView, QVBoxLayout, QListWidget, QListWidgetItem,
                               QSplitter, QBoxLayout, QAbstractItemView,
                               QSizePolicy)
from PySide2.QtCharts import QtCharts
from PySide2.QtCore import QObject, Signal, Slot, QPoint

from communication import communication

class Table(QTableView):
    def __init__(self, model):
        QTableView.__init__(self)
        self.setModel(model)

        # QTableView Headers
        resize = QHeaderView.ResizeToContents
        self.horizontal_header = self.horizontalHeader()
        self.vertical_header = self.verticalHeader()
        self.horizontal_header.setSectionResizeMode(resize)
        self.vertical_header.setSectionResizeMode(resize)
        self.horizontal_header.setStretchLastSection(True)

        communication.model.connect(self.recv)

    @Slot(object)
    def recv(self, model):
        print("new model")
        self.setModel(model)