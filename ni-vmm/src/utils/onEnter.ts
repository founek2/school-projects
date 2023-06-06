export const onEnterRun = (Fn: (e: React.KeyboardEvent) => void) => (e: React.KeyboardEvent) => {
    if (e.key === 'Enter') Fn(e);
};
