import Masonry from '@mui/lab/Masonry';
import { CardMedia, CircularProgress, Dialog, DialogContent, IconButton, Paper, Typography } from '@mui/material';
import { Box, Stack } from '@mui/system';
import React, { useEffect, useState } from 'react';
import { useAppSelector } from '../../hooks';
import { useOrdering } from '../../hooks/useOrdering';
import { getPhotos, getRankingValue } from '../../selectors/getters';
import VisibilityIcon from '@mui/icons-material/Visibility';
import VisibilityOffIcon from '@mui/icons-material/VisibilityOff';
import { PhotoStore } from '../../store/slices/photosSlice';

function getWindowDimensions() {
    const { innerWidth: width, innerHeight: height } = window;
    return {
        width,
        height,
    };
}

export function MediaGrid() {
    const [windowDimensions, setWindowDimensions] = useState(getWindowDimensions());
    const [selectedPhotoIdx, setSelectedPhotoIdx] = useState<number>();
    const photos = useAppSelector(getPhotos);
    const [orderedPhotos, distances, finalDistance] = useOrdering(photos.data);
    const [showDebug, setShowDebug] = useState(true);
    const [loading, setLoading] = useState(false);
    const disabledRanking = useAppSelector(getRankingValue('disabled'));

    function setSelectedPhoto(photo: PhotoStore, idx: number) {
        setSelectedPhotoIdx(idx);
        setLoading(true);
    }

    useEffect(() => {
        function handleResize() {
            setWindowDimensions(getWindowDimensions());
        }

        window.addEventListener('resize', handleResize);
        return () => window.removeEventListener('resize', handleResize);
    }, []);

    const data = disabledRanking ? photos.data : orderedPhotos;

    return (
        <>
            <Paper sx={{ padding: 3, paddingRight: 1, paddingTop: 1 }}>
                <Stack direction="row">
                    <Typography variant="h5">Results {photos.perPage > 0 ? `(${photos.perPage})` : ''}</Typography>
                    <IconButton onClick={() => setShowDebug(!showDebug)}>
                        {showDebug ? <VisibilityOffIcon /> : <VisibilityIcon />}
                    </IconButton>
                </Stack>
                <Masonry columns={Math.floor(windowDimensions.width / 400) || 1} spacing={2}>
                    {data.map((photo, idx) => (
                        <Box key={photo.id} sx={{ position: 'relative' }} onClick={() => setSelectedPhoto(photo, idx)}>
                            <img
                                src={photo.url_c}
                                alt={photo.title}
                                loading="lazy"
                                style={{
                                    borderBottomLeftRadius: 4,
                                    borderBottomRightRadius: 4,
                                    display: 'block',
                                    width: '100%',
                                }}
                            />
                            {showDebug ? (
                                <>
                                    <Typography
                                        sx={{
                                            position: 'absolute',
                                            right: 0,
                                            bottom: 0,
                                            color: 'red',
                                            backgroundColor: 'rgba(0, 0, 0, 0.6)',
                                        }}
                                    >
                                        {Object.entries(distances).map(([key, distances]) => (
                                            <span key={`${key}-${idx}`}>
                                                {key}: {distances[idx].toFixed()}
                                                <br />
                                            </span>
                                        ))}
                                        final: {finalDistance[idx].toFixed(2)}
                                    </Typography>
                                    <Typography sx={{ display: 'none' }} className={'tags'}>
                                        {photo.tags.join(',')}
                                    </Typography>
                                </>
                            ) : null}
                        </Box>
                    ))}
                </Masonry>
            </Paper>
            <Dialog
                open={selectedPhotoIdx !== undefined}
                onClose={() => setSelectedPhotoIdx(undefined)}
                PaperProps={{ sx: { maxWidth: '70%' } }}
            >
                <DialogContent sx={{ padding: 0 }}>
                    {loading ? <CircularProgress /> : null}
                    <CardMedia
                        component="img"
                        onLoad={() => setLoading(false)}
                        image={
                            selectedPhotoIdx === undefined
                                ? undefined
                                : orderedPhotos[selectedPhotoIdx].url_o || orderedPhotos[selectedPhotoIdx].url_c
                        }
                    />
                </DialogContent>
            </Dialog>
        </>
    );
}
