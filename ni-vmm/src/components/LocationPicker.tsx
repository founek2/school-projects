import Masonry from '@mui/lab/Masonry';
import { Dialog, DialogTitle, IconButton, Paper, Rating, TextField, Typography } from '@mui/material';
import React, { useEffect, useRef, useState } from 'react';
import RoomIcon from '@mui/icons-material/Room';
import { GoogleMap, useJsApiLoader, Marker, Autocomplete, StandaloneSearchBox } from '@react-google-maps/api';

const containerStyle = {
    width: '800px',
    height: '600px',
};

const centerDefault = {
    lat: -3.745,
    lng: -38.523,
};
const mapConfig: Parameters<typeof useJsApiLoader>[0] = {
    id: 'google-map-script',
    googleMapsApiKey: 'AIzaSyCbdlFuMxucBaYf6usybQN1d5x5-r0ZURs',
    libraries: ['geometry'],
};
type Point = { lng: number; lat: number };
export interface LocationPickerProps {
    onChange: (e: { target: { value: Point } }) => void;
    value?: Point;
    markers?: { lng: number; lat: number; id: string; icon: string }[];
}
export function LocationPicker({ onChange, value, markers }: LocationPickerProps) {
    const [open, setOpen] = useState(false);
    const [center, setCenter] = useState<Point>(centerDefault);
    const [zoom, setZoom] = useState(2);
    const { isLoaded } = useJsApiLoader(mapConfig);
    const ref = useRef<NodeJS.Timeout>();
    const [map, setMap] = React.useState<google.maps.Map | null>(null);

    useEffect(() => {
        if (value) setCenter(value);
    }, []);

    const onUnmount = React.useCallback(function callback(map: any) {
        setMap(null);
    }, []);
    const onLoad = React.useCallback(function callback(map: google.maps.Map) {
        setMap(map);
    }, []);

    const onZoom = React.useCallback(
        function callback() {
            console.log('map', map);
            if (map && map.getZoom()) setZoom(map.getZoom()!);
        },
        [map]
    );

    function onClick(e: any) {
        if (e.domEvent.type === 'mouseup') {
            clearTimeout(ref.current);
        } else {
            ref.current = setTimeout(() => {
                onChange({ target: { value: { lat: e.latLng.lat(), lng: e.latLng.lng() } } });
            }, 400);
        }
    }
    function onClose() {
        setOpen(false);
        if (value) setTimeout(() => setCenter(value), 400);
    }
    return (
        <>
            <IconButton onClick={() => setOpen(true)}>
                <RoomIcon />
            </IconButton>
            <Dialog open={open} onClose={onClose}>
                <DialogTitle>Choose location</DialogTitle>
                {isLoaded ? (
                    <GoogleMap
                        mapContainerStyle={containerStyle}
                        center={center}
                        zoom={zoom}
                        onZoomChanged={onZoom}
                        onLoad={onLoad}
                        onUnmount={onUnmount}
                        // onClick={onClick}
                        onMouseDown={onClick}
                        onMouseUp={onClick}
                        onDragStart={() => clearTimeout(ref.current)}
                        // onZoomChanged={zoom => }
                    >
                        {/* Child components, such as markers, info windows, etc. */}
                        {value ? <Marker position={value} /> : null}
                        {markers?.map((loc) => (
                            <Marker
                                key={loc.id}
                                position={loc}
                                icon={{
                                    url: loc.icon,
                                    scaledSize: new google.maps.Size(40, 40), // scaled size
                                    origin: new google.maps.Point(0, 0), // origin
                                    anchor: new google.maps.Point(0, 0), // anchor
                                }}
                            />
                        ))}
                    </GoogleMap>
                ) : null}
            </Dialog>
        </>
    );
}
