from PySide2.QtWidgets import (QListWidget,QVBoxLayout,QWidget,QHBoxLayout,QLabel, QListWidgetItem, QAbstractItemView,
                               QPushButton)
from communication import communication

ORDERING = ("Asc", "desc")


class Picker(QWidget):
    def __init__(self, data):
        QWidget.__init__(self)
        self.list = QListWidget()
        self.list.setSelectionMode(QAbstractItemView.MultiSelection)
        self.list.itemClicked.connect(self.print_item_text)

        self.order1 = QListWidget()
        self.order2 = QListWidget()
        self.button = QPushButton("Process")
        self.button.clicked.connect(self.send)

        self.items = []
        self.items1 = []
        self.items2 = []
        for name in data.columns[1:4]:
            self.items.append(QListWidgetItem(name, self.list))

        for name in ORDERING:
            self.items1.append(QListWidgetItem(name, self.order1))
            self.items2.append(QListWidgetItem(name, self.order2))

        self.list.setItemSelected(self.items[0], True)
        self.list.setItemSelected(self.items[1], True)

        self.order1.setCurrentItem(self.items1[0])
        self.order2.setCurrentItem(self.items2[0])

        self.main_layout = QHBoxLayout()
        label1 = QLabel("Kategorie")
        self.label2 = QLabel(f"Důležitost pro {self.items[0].text()}")
        self.label3 = QLabel(f"Důležitost pro {self.items[1].text()}")

        layout1 = QVBoxLayout()
        layout1.addWidget(label1)
        layout1.addWidget(self.list)

        layout2 = QVBoxLayout()
        layout2.addWidget(self.label2)
        layout2.addWidget(self.order1)

        layout3 = QVBoxLayout()
        layout3.addWidget(self.label3)
        layout3.addWidget(self.order2)

        self.main_layout.addLayout(layout1, 4)
        self.main_layout.addLayout(layout2, 1)
        self.main_layout.addLayout(layout3, 1)
        self.main_layout.addWidget(self.button)

        self.setLayout(self.main_layout)


    def print_item_text(self):
        items = self.list.selectedItems()
        if len(items) <= 2:
            pass
        else:
            for item in items[0:-2]:
                item.setSelected(False)

        selected = self.get_selected_categories()
        if len(selected) == 2:
            communication.categories.emit(selected)
            self.label2.setText(f"Důležitost pro {selected[0]}")
            self.label3.setText(f"Důležitost pro {selected[1]}")
        print(self.get_selected_categories())

    def get_selected_categories(self):
        return [item.text() for item in self.list.selectedItems() if self.list.isItemSelected(item)]

    def send(self):
        communication.submit.emit(self.get_selected_categories(), self.order1.currentItem().text(), self.order2.currentItem().text())
