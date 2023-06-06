from PySide2.QtCore import Qt
from PySide2.QtGui import QPainter
from PySide2.QtCharts import QtCharts
from communication import communication
from PySide2.QtCore import Slot
from callout import Callout
from algorithms.sweep_plane import sweep_plane_alg, make_edges
import pandas as pd
from table_car_model import CustomTableModel
from algorithms.brute_force import brute_force_alg
from algorithms.divide_and_conquer import divide_and_conquer_alg
import time

FORMATTERS = {
    "Id": ("%d"),
    "Price": ("$%'d"),
    "Year": ("%d"),
    "Mileage": ("%'d"),
    "City": ("%s"),
    "State": ("%s"),
    "Vin": ("%s"),
    "Make": ("%s"), "Model": ("%s")}


class Graph(QtCharts.QChartView):
    def __init__(self, model):
        QtCharts.QChartView.__init__(self)
        self.model = model
        # Creating QChart
        self.chart = QtCharts.QChart()
        self.chart.setAnimationOptions(QtCharts.QChart.NoAnimation)

        # Create QLineSeries

        self.x_name = "Price"
        self.y_name = "Year"
        self.add_series(model.df)
        self.setContentsMargins(90, 90, 10, 10)
        self.setRenderHint(QPainter.Antialiasing)
        self.setChart(self.chart)

        communication.submit.connect(self.recv)
        communication.categories.connect(self.recv_categories)
        self._tooltip = Callout(self.chart)

    def add_series(self, df, replacement=None):
        x_name = self.x_name
        y_name = self.y_name

        if replacement:
            self.chart.removeSeries(replacement)

        if hasattr(self, "axis_x"):
            self.chart.removeAxis(self.axis_x)
            self.chart.removeAxis(self.axis_y)

        if hasattr(self, "skyline"):
            self.chart.removeSeries(self.skyline)
            delattr(self, "skyline")

        self.series = QtCharts.QScatterSeries()
        self.series.setName(f"{x_name}/{y_name}")
        # Filling QLineSeries
        for i, row in df.iterrows():
            # Getting the data

            x = int(row[x_name])
            y = int(row[y_name])

            self.series.append(x, y)

        self.chart.addSeries(self.series)

        # Setting X-axis
        self.axis_x = QtCharts.QValueAxis()
        self.axis_x.setTickCount(10)
        self.axis_x.setTitleText(x_name)
        self.axis_x.setLabelFormat(FORMATTERS[x_name])
        self.chart.addAxis(self.axis_x, Qt.AlignBottom)
        self.series.attachAxis(self.axis_x)
        add_padding(self.axis_x)
        # Setting Y-axis
        self.axis_y = QtCharts.QValueAxis()
        self.axis_y.setTickCount(10)
        self.axis_y.setTitleText(y_name)
        self.axis_y.setLabelFormat(FORMATTERS[y_name])
        self.chart.addAxis(self.axis_y, Qt.AlignLeft)
        self.series.attachAxis(self.axis_y)
        add_padding(self.axis_y)

        # Getting the color from the QChart to use it on the QTableView
        # self.model.color = "{}".format(self.series.pen().color().name())
        #        print("color", self.model.color)
        self.series.hovered.connect(self.tooltip)

    def add_skyline(self, stack):
        x_name = self.x_name
        y_name = self.y_name

        if hasattr(self, "skyline"):
            self.chart.removeSeries(self.skyline)

        self.skyline = QtCharts.QLineSeries()
        # self.skyline.setColor("#ff1f1f")
        self.skyline.setName(f"skyline {x_name}/{y_name}")
        # Filling QLineSeries
        for row in stack:
            # Getting the data

            x = int(row[x_name])
            y = int(row[y_name])

            self.skyline.append(x, y)

        self.chart.addSeries(self.skyline)

        self.skyline.attachAxis(self.axis_x)
        self.skyline.attachAxis(self.axis_y)


    @Slot(list, str, str)
    def recv(self, arr, order1, order2):
        print("got", arr, order1, order2)
        x_name, y_name = arr
        df = self.model.df

        df.sort_values(by=[x_name, y_name], inplace=True, ascending=[order1 == "Asc", order2 == "Asc"])

        start = time.time()
        stack = sweep_plane_alg(df, x_name, y_name, order1 == "Asc", order2 == "Asc")
        print("Sweep plane alg - ",  time.time() - start, "sec")

        start = time.time()
        stack = brute_force_alg(df, x_name, y_name, order1 == "Asc", order2 == "Asc")
        print("Brute force alg - ", time.time() - start, "sec")

        start = time.time()
        stack = divide_and_conquer_alg(df, x_name, y_name, order1 == "Asc", order2 == "Asc")
        print("Divide and conc alg - ", time.time() - start, "sec")

        print("items count DAC: ", len(stack))
        # print("first in stack",stack[0])
        new = pd.concat(stack, axis=1, join="inner")
        self.add_skyline(make_edges(stack, x_name, y_name, order1 == "Asc", order2 == "Asc"))
        new = new.transpose().astype("int64")       # // problematic for non number values
        subset = df.loc[df.Id.isin(new.Id)]
        # print(subset)
        communication.model.emit(CustomTableModel(subset))

    @Slot(list)
    def recv_categories(self, arr):
        if len(arr) == 2:
            self.x_name = arr[0]
            self.y_name = arr[1]
            self.add_series(self.model.df, self.series)

    def tooltip(self, point, state):
        if self._tooltip == 0:
            self._tooltip = Callout(self._chart)

        if state:
            df = self.model.df
            data = df.loc[(df[self.x_name] == point.x()) & (df[self.y_name] == point.y())]
            out = ""
            for idx, item in data.iterrows():
                out += f'Make: {item["Make"]}\n' \
                       f'Model: {item["Model"]}\n' \
                       f'Price: ${item["Price"]:,.0f}\n' \
                       f'Mileage: {item["Mileage"]:,}\n' \
                       f'Year: {item["Year"]}'
                count_row = data.shape[0]
                if count_row > 1:
                    out += "----------"

            self._tooltip.setText(out)
            self._tooltip.setAnchor(point)
            self._tooltip.setZValue(11)
            self._tooltip.updateGeometry()
            self._tooltip.show()
        else:
            self._tooltip.hide()


def add_padding(axis):
    relative = axis.max() - axis.min()
    axis.setMin(axis.min() - relative * 0.05)
    axis.setMax(axis.max() + relative * 0.05)
