import { createAlova, useRequest } from 'alova';
import GlobalFetch from 'alova/GlobalFetch';
import ReactHook from 'alova/react';

// 1. Create an alova instance
export const alovaInstance = createAlova({
    // ReactHook is used to create ref status, including request status loading, response data data, request error object error, etc.
    statesHook: ReactHook,

    // request adapter, it is recommended to use the fetch request adapter
    requestAdapter: GlobalFetch(),
    responded: {
        onSuccess: async (response, method) => {
            if (response.status >= 400) {
                throw new Error(response.statusText);
            }
            const json = await response.json();

            return json;
        },
    },
});
