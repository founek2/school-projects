import React, { useEffect, useState } from 'react';
import styled from '@emotion/styled';
import * as utils from './utils.js';
import { useRequest } from 'alova';
import { alovaInstance } from './api/index.js';
import Loader from './Loader.jsx';
import { css } from '@emotion/react';

const Input = styled.input`
    padding: 0.7rem 1rem;
    font-size: 1.3rem;
    border-radius: 4px;
`;

const Center = styled.div`
    display: flex;
    align-items: center;
    justify-content: center;
`;

export default function TodoCreator({ onNewTodo, userId }) {
    const [value, setValue] = useState();
    const { loading, data, error, send } = useRequest(
        alovaInstance.Post(
            'https://dummyjson.com/todos/add',
            {
                todo: value,
                completed: false,
                userId,
            },
            {
                headers: {
                    'Content-Type': 'application/json;charset=UTF-8',
                },
            }
        ),
        { immediate: false }
    );

    useEffect(() => {
        if (data) {
            setValue('');
            onNewTodo(data);
        }
    }, [data]);

    return (
        <Center>
            <Input
                disabled={loading}
                autoFocus
                placeholder="Start typing..."
                onKeyDown={utils.onEnter(send)}
                value={value}
                onChange={(e) => setValue(e.target.value)}
            />
            {loading ? <Loader /> : null}
        </Center>
    );
}
