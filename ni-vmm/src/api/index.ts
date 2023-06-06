import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react';

// Create our baseQuery instance
const baseQuery = fetchBaseQuery({
    baseUrl: 'https://www.flickr.com/services/rest/',
});

/**
 * Create a base API to inject endpoints into elsewhere.
 * Components using this API should import from the injected site,
 * in order to get the appropriate types,
 * and to ensure that the file injecting the endpoints is loaded
 */
export const api = createApi({
    /**
     * A bare bones base query would just be `baseQuery: fetchBaseQuery({ baseUrl: '/' })`
     */
    baseQuery,
    /**
     * Tag types must be defined in the original API definition
     * for any tags that would be provided by injected endpoints
     */
    tagTypes: ['SignIn', 'Devices', 'AuthProviders', 'UserAuthTypes'],
    /**
     * This api has endpoints injected in adjacent files,
     * which is why no endpoints are shown below.
     */
    endpoints: () => ({}),
});
