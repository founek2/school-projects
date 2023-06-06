import { api } from './';

export interface Photo {
    id: string;
    owner: string;

    secret: string;
    server: string;
    farm: 66;
    title: string;
    ispublic: 1 | 0;
    isfriend: 1 | 0;
    isfamily: 1 | 0;
    url_c: string;
    url_o: string;
    url_sq: string;
    latitude: string;
    longitude: string;
    tags: string; // space separated list
    width_o: number; // width of the original
    datetaken: string;
    ownername: string;
    views: string;
}
export interface SearchResult {
    photos: {
        page: number;
        pages: string;
        perpage: number;
        total: string;
        stat: 'ok';
        photo: Photo[];
    };
}

interface SearchQuery {
    text: string;
    tags?: string[];
    min_upload_date?: Date;
    max_upload_date?: Date;
    min_taken_date?: Date;
    max_taken_date?: Date;
    sort?:
        | 'date-posted-desc'
        | 'date-posted-asc'
        | 'date-taken-asc'
        | 'date-taken-desc'
        | 'interestingness-desc'
        | 'interestingness-asc'
        | 'relevance';
    /**
     * 1 for photos only.
     * 2 for screenshots only.
     * 3 for 'other' only.
     * 4 for photos and screenshots.
     * 5 for screenshots and 'other'.
     * 6 for photos and 'other'.
     * 7 for photos, screenshots, and 'other' (all).
     */
    content_type?: 1 | 2 | 3 | 4 | 5 | 6 | 7;
    media?: 'videos' | 'photos' | 'all';
    has_geo?: 1 | 0;
    extras?: (
        | 'description'
        | 'license'
        | 'date_upload'
        | 'date_taken'
        | 'owner_name'
        | 'icon_server'
        | 'original_format'
        | 'last_update'
        | 'geo'
        | 'tags'
        | 'machine_tags'
        | 'o_dims'
        | 'views'
        | 'media'
        | 'path_alias'
        | 'url_sq'
        | 'url_t'
        | 'url_s'
        | 'url_q'
        | 'url_m'
        | 'url_n'
        | 'url_z'
        | 'url_c'
        | 'url_l'
        | 'url_o'
    )[];
    /** default 100, max 500 */
    per_page?: number;
}
export const searchApi = api.injectEndpoints({
    endpoints: (build) => ({
        photos: build.query<SearchResult['photos'], SearchQuery>({
            query: (args) =>
                '?api_key=b17fe7357f4158f8602e59829f17c04c&method=flickr.photos.search&format=json&nojsoncallback=1&' +
                Object.entries(args)
                    .map(([key, value]) => `${key}=${Array.isArray(value) ? value.join(',') : value}`)
                    .join('&'),
            // query: ({ text }) =>
            //     `?api_key=b17fe7357f4158f8602e59829f17c04c&method=flickr.photos.search&text=${text}&format=json&nojsoncallback=1`,
            providesTags: ['Devices'],
            transformResponse: (data: SearchResult) => data.photos,
        }),
    }),
});

export const { useLazyPhotosQuery, usePhotosQuery } = searchApi;
