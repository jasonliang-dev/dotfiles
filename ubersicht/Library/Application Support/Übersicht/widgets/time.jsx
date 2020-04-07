import Base from "./Base.jsx";

export const command = 'date "+%I:%M %p"';

export const refreshFrequency = 1000;

export const className = `
  top: 1rem;
  right: 1rem;
`;

export const render = ({ output, error }) => {
  return <Base>{output}</Base>;
};
