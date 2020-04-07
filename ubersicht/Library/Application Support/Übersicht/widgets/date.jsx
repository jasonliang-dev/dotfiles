import Base from "./Base.jsx";

export const command = 'date "+%a, %b %d"';

export const refreshFrequency = 60000;

export const className = `
  top: 1rem;
  left: 50%;
  transform: translateX(-50%);
`;

export const render = ({ output, error }) => {
  return <Base>{output}</Base>;
};
