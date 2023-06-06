from PySide2.QtCore import Qt, QAbstractTableModel, QModelIndex
from PySide2.QtGui import QColor

CATEGORIES = ("Id","Price","Year","Mileage","City","State","Vin","Make","Model")

class CustomTableModel(QAbstractTableModel):
    def __init__(self, data=None):
        QAbstractTableModel.__init__(self)
        self.load_data(data)

    def load_data(self, data):
        self.input_dates = data["City"].values
        self.input_magnitudes = data["Model"].values
        self.df = data

        self.column_count = len(data.columns)
        self.row_count = len(self.input_magnitudes)

    def rowCount(self, parent=QModelIndex()):
        return self.row_count

    def columnCount(self, parent=QModelIndex()):
        return self.column_count

    def headerData(self, section, orientation, role):
        if role != Qt.DisplayRole:
            return None
        if orientation == Qt.Horizontal:
            return CATEGORIES[section]
        else:
            return "{}".format(section)

    def categoryToIdx(self, name):
        return CATEGORIES.index(name)

    def data(self, index, role=Qt.DisplayRole):
        column = index.column()
        row = index.row()

        if role == Qt.DisplayRole:
            return "{}".format(self.df.iloc[row, column])
        elif role == Qt.BackgroundRole:
            return QColor(Qt.white)
        elif role == Qt.TextAlignmentRole:
            return Qt.AlignRight

        return None
