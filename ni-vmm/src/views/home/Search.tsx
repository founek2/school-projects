import SearchIcon from '@mui/icons-material/Search';
import {
    Box,
    Card,
    CardContent,
    Chip,
    CircularProgress,
    IconButton,
    InputAdornment,
    OutlinedInput,
    Stack,
    Typography,
} from '@mui/material';
import React, { useState } from 'react';
import { useLazyPhotosQuery } from '../../api/search';
import { onEnterRun } from '../../utils/onEnter';

const PER_PAGE = [50, 100, 200, 500];

export function Search() {
    const [perPage, setPerPage] = useState(50);
    const [searchText, setSearchText] = useState('');
    const [queryPhotos, { isFetching }] = useLazyPhotosQuery();

    function handleSearch() {
        queryPhotos({
            text: searchText,
            extras: [
                'url_c',
                'url_o',
                'url_sq',
                'geo',
                'description',
                'tags',
                'views',
                'date_taken',
                'owner_name',
                'views',
            ],
            media: 'photos',
            has_geo: 1,
            per_page: perPage,
        });
    }

    return (
        <Card>
            <CardContent>
                <Stack alignItems="center">
                    <Typography variant="h5" component="div">
                        Browse all of photos around the world
                    </Typography>

                    <OutlinedInput
                        placeholder="Type to search..."
                        sx={{ maxWidth: 400 }}
                        fullWidth
                        value={searchText}
                        onKeyDown={onEnterRun(handleSearch)}
                        onChange={(e: any) => setSearchText(e.target.value)}
                        endAdornment={
                            <InputAdornment position="end">
                                <IconButton aria-label="toggle password visibility" onClick={handleSearch} edge="end">
                                    {isFetching ? <CircularProgress size={20} /> : <SearchIcon />}
                                </IconButton>
                            </InputAdornment>
                        }
                    />
                </Stack>
                <Stack direction="row" justifyContent="center">
                    {PER_PAGE.map((v) => (
                        <Chip
                            key={v}
                            label={v}
                            variant={v === perPage ? 'outlined' : 'filled'}
                            onClick={() => setPerPage(v)}
                        />
                    ))}
                </Stack>
            </CardContent>
        </Card>
    );
}
