function deg2rad(deg: number) {
    return deg * (Math.PI / 180);
}

export type Point = { lat: number; lng: number };
export function getDistanceFromLatLonInKm(point1: Point, point2: Point) {
    // Radius of the earth in km
    var R = 6371;

    var radLat = deg2rad(point2.lat - point1.lat);
    var radLng = deg2rad(point2.lng - point1.lng);
    var a =
        Math.sin(radLat / 2) * Math.sin(radLat / 2) +
        Math.cos(deg2rad(point1.lat)) * Math.cos(deg2rad(point2.lat)) * Math.sin(radLng / 2) * Math.sin(radLng / 2);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    var d = R * c; // Distance in kilometers
    return d;
}
