import { css } from "uebersicht";

const container = css`
  background: rgba(0, 0, 0, 0.5);
  -webkit-backdrop-filter: blur(20px);
  color: white;
  font-family: Iosevka;
  font-weight: 700;
  padding: 0.5rem 1.25rem;
  border-radius: 0.375rem;
  margin: 0 0.25rem;
`;

export default function Base({ children }) {
  return <div className={container}>{children}</div>;
}
