from PySide2.QtCore import Slot, qApp
from PySide2.QtGui import QKeySequence
from PySide2.QtWidgets import QMainWindow, QAction

"""
sweep algorithm
seřadit podle osy x
jít od začátku:
    zjistit jestli ve stacku není menší hodnota -> pokud ano tak pop
    push na stack
    
na konci jsou ve stacku -> easy peasy
"""
class MainWindow(QMainWindow):
    def __init__(self, widget):
        QMainWindow.__init__(self)
        self.setWindowTitle("Skyline computation")
        self.setCentralWidget(widget)
        # Menu
        self.menu = self.menuBar()
        self.file_menu = self.menu.addMenu("File")

        ## Exit QAction
        exit_action = QAction("Exit", self)
        exit_action.setShortcut(QKeySequence.Quit)
        exit_action.triggered.connect(self.close)

        self.file_menu.addAction(exit_action)

        # Status Bar
        self.status = self.statusBar()
        self.status.showMessage("Data loaded and plotted")

        # Window dimensions
        desktop = qApp.desktop()
        geometry = desktop.availableGeometry(self)

        width = geometry.width() * 0.9
        height = geometry.height() * 0.8

        self.resize(width, height)

