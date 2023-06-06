import { Box } from '@mui/material';
import React from 'react';
import { MediaGrid } from './home/MediaGrid';
import { Reranking } from './home/Reranking';
import { Search } from './home/Search';

export function Home() {
    return (
        <>
            <Search />
            <Box sx={{ height: 10 }} />
            <Reranking />
            <Box sx={{ height: 10 }} />
            <MediaGrid />
        </>
    );
}
