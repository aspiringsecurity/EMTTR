import PropTypes from 'prop-types';

function TabPanel({
  children,
  value,
  index,
}) {
  return (
    <div
      role="tabpanel"
      hidden={value !== index}
    >
      {value === index && (
        <div>{children}</div>
      )}
    </div>
  );
}

TabPanel.defaultProps = {
  children: null,
};

TabPanel.propTypes = {
  children: PropTypes.node,
  index: PropTypes.number.isRequired,
  value: PropTypes.number.isRequired,
};

export default TabPanel;
