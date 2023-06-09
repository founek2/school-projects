// http://techslides.com/convert-csv-to-json-in-javascript

export default function csvToArray(csv) {

    var lines = csv.split("\n");

    var result = [];

    var headers = lines[0].split(",");

    for (var i = 1; i < lines.length; i++) {

        var obj = {};
        var currentline = lines[i].split(",");

        for (var j = 0; j < headers.length; j++) {
            if (isNaN(currentline[j]))
                obj[headers[j]] = currentline[j];
            else
                obj[headers[j]] = Number(currentline[j]);
        }

        result.push(obj);

    }

    //return result; //JavaScript object
    return result; //JSON
}