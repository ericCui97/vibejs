import { useState } from 'react';
import './App.css';

function App() {
  const [code, setCode] = useState('let x = 10;');
  const [tokens, setTokens] = useState(null);
  const [ast, setAst] = useState(null);
  const [error, setError] = useState(null);
  const [activeTab, setActiveTab] = useState('tokens'); // 'tokens' or 'ast'

  const handleCompile = async () => {
    setError(null);
    setTokens(null);
    setAst(null);
    
    try {
      // Fetch Tokens
      const tokenRes = await fetch('http://localhost:3000/compile', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ code, mode: 'token' }),
      });
      const tokenData = await tokenRes.json();
      if (!tokenRes.ok) throw new Error(tokenData.error || 'Token Compilation failed');
      setTokens(tokenData);

      // Fetch AST
      const astRes = await fetch('http://localhost:3000/compile', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ code, mode: 'ast' }),
      });
      const astData = await astRes.json();
      if (!astRes.ok) throw new Error(astData.error || 'AST Compilation failed');
      setAst(astData);

    } catch (err) {
      setError(err.message);
    }
  };

  const getTokenClass = (token) => {
    if (typeof token === 'string') return 'token-keyword';
    if (token.Identifier) return 'token-identifier';
    if (token.Number) return 'token-number';
    if (token.String) return 'token-string';
    return 'token-default';
  };

  const formatToken = (token) => {
    if (typeof token === 'string') return token;
    const key = Object.keys(token)[0];
    return `${key}(${token[key]})`;
  };

  return (
    <div className="container">
      <h1>JS Compiler Visualizer</h1>
      
      <div className="editor-section">
        <textarea
          value={code}
          onChange={(e) => setCode(e.target.value)}
          placeholder="Enter JS code here..."
          rows={10}
        />
        <button onClick={handleCompile}>Compile</button>
      </div>

      {error && <div className="error">Error: {error}</div>}

      <div className="tabs">
        <button 
          className={activeTab === 'tokens' ? 'active' : ''} 
          onClick={() => setActiveTab('tokens')}
        >
          Tokens
        </button>
        <button 
          className={activeTab === 'ast' ? 'active' : ''} 
          onClick={() => setActiveTab('ast')}
        >
          AST
        </button>
      </div>

      <div className="output-section">
        {activeTab === 'tokens' && tokens && (
          <>
            <h2>Tokens ({tokens.length})</h2>
            <div className="tokens-list">
              {tokens.map((token, index) => (
                <div key={index} className={`token-item ${getTokenClass(token)}`}>
                  {formatToken(token)}
                </div>
              ))}
            </div>
          </>
        )}

        {activeTab === 'ast' && ast && (
          <>
            <h2>Abstract Syntax Tree</h2>
            <div className="ast-view">
              <pre>{JSON.stringify(ast, null, 2)}</pre>
            </div>
          </>
        )}
      </div>
    </div>
  );
}

export default App;
