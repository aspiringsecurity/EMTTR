import Dao from './components/pages/Dao';
import Defi from './components/pages/Defi';
import NotFound from './components/pages/NotFound';
import { Route, Switch } from 'react-router-dom';

function App() {
  return (
    <div className="bg-gray-900">
        <Switch>
            <Route path='/' exact>
                <Defi />
            </Route>

            <Route path='/dao' exact>
                <Dao />
            </Route>

            <Route path="*">
              <NotFound />
            </Route>
        </Switch>
    </div>
  );
}

export default App;
