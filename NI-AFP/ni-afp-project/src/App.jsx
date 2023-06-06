import React, { useEffect, useState } from 'react';
import Elm from 'react-elm-components';
import TodoList from './TodoList';
import UserSelect from './UserSelect';
import TodoCreator from './TodoCreator';
import styled from '@emotion/styled';

const Container = styled.div`
    text-align: center;
    display: flex;
    flex-direction: column;
`;

const PageContainer = styled.div`
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
`;

const H1 = styled.h1`
    font-size: 10vh;
    color: white;
`;

export default function App() {
    const [newTodoPort, setNewTodoPort] = useState();
    const [userIdSubPort, setUserIdSubPort] = useState();
    const [userIdSendPort, setUserIdSendPort] = useState();
    const [userId, setUserId] = useState();

    useEffect(() => {
        if (userIdSubPort) userIdSubPort?.subscribe(setUserId);
    }, [userIdSubPort]);

    useEffect(() => {
        if (userId) userIdSendPort?.send(userId);
    }, [userId]);

    return (
        <PageContainer>
            <Container>
                <H1>TODO App</H1>
                <TodoCreator userId={userId} onNewTodo={newTodoPort?.send} />
                <div>
                    Pick userId
                    <Elm
                        src={UserSelect.Elm.UserSelect}
                        flags={5}
                        ports={(ports) => setUserIdSubPort(ports.changeUserId)}
                    />
                </div>
            </Container>
            <Elm
                src={TodoList.Elm.TodoList}
                ports={(ports) => {
                    setNewTodoPort(ports.newTodo);
                    setUserIdSendPort(ports.userIdChange);
                }}
            />
        </PageContainer>
    );
}
