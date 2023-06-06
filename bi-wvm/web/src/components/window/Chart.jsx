import * as React from "react";
import { Chart } from "react-google-charts";

const ExampleChart = ({ axis, data, title }) => {
    return (<Chart
        width='100%'
        height={'700px'}
        chartType="ComboChart"
        loader={<div>Loading Chart</div>}
        // data={[
        //     ['Age', 'Weight'],
        //     [8, 12],
        //     [4, 5.5],
        //     [11, 14],
        //     [4, 5],
        //     [3, 3.5],
        //     [6.5, 7],
        // ]}
        data={data}
        options={{
            title: title,

            hAxis: { title: axis.x.label },
            vAxis: { title: axis.y.label },
            // legend: 'none',
            'chartArea': { 'width': '80%', 'height': '80%'},
            areaOpacity: 0.24,
            colorAxis: { colors: ["#3f51b5", "#2196f3", "#03a9f4", "#00bcd4"] },
            backgroundColor: 'transparent',
            areaOpacity: 0.24,
            // lineWidth: 1,
            colors: ["#3f51b5", "#2196f3", "#03a9f4", "#00bcd4", "#009688", "#4caf50", "#8bc34a", "#cddc39"],
            explorer: {
                actions: ['dragToZoom', 'rightClickToReset'],
                // actions: ['dragToPan'],
                axis: 'horizontal',
                keepInBounds: true,
                maxZoomIn: 0.1,
                zoomDelta: 1.1
            },
            tooltip: { isHtml: true },

            seriesType: 'scatter',
            series: { 1: { type: 'line', pointSize: 0 } },
            interpolateNulls: true,
            pointSize: 7

        }}
        rootProps={{ 'data-testid': '1' }}
    />)
};
export default ExampleChart;